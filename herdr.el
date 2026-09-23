;;; herdr.el --- Herdr TUI integration for Emacs/Ghostel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ngoc Nguyen

;; Author: Ngoc Nguyen
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (ghostel "0.1") (transient "0.4"))
;; Keywords: terminals, tools
;; URL: https://github.com/baongoc124/emacs-herdr
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Herdr <https://herdr.dev> is a tmux-like multiplexer for AI coding
;; agents.  This package runs its full TUI (mouse UI included) in one
;; Ghostel terminal buffer and adds the few things Emacs does faster:
;;
;;   `herdr'               pop to the *herdr* buffer, launching it if needed
;;   `herdr-hide'          hide that window, restoring what it displaced
;;   `herdr-menu'          transient menu; `herdr-menu-key' opens it globally
;;   `herdr-switch-agent'  completing-read over agents, sorted by attention
;;   `herdr-new-agent'     start an agent for the project (new tab or workspace)
;;   `herdr-goto-blocked'  focus the first agent waiting for input
;;   `herdr-send-region'   stage (or C-u submit) the region as a code block
;;   `herdr-mode'          poll timer and mode-line blocked count
;;
;; Everything goes through the `herdr' CLI (>= 0.8), never the raw socket.

;;; Code:

(require 'ghostel)
(require 'project)
(require 'json)
(require 'transient)
(require 'vc-git)

;;; Customization

(defgroup herdr nil
  "Herdr TUI integration."
  :group 'tools
  :prefix "herdr-")

(defcustom herdr-executable "herdr"
  "Path to the herdr binary."
  :type 'string)

(defcustom herdr-poll-interval 2
  "Seconds between snapshot polls."
  :type 'natnum)

(defcustom herdr-default-agent-kind "claude"
  "Default agent kind for `herdr-new-agent'."
  :type 'string)

(defcustom herdr-sync-workspace-labels 'project
  "How to rename the workspace of each agent.
`project' uses the project.el name of the agent's cwd; `project-session'
appends the agent's session name (its terminal title); nil leaves labels
alone."
  :type '(choice (const :tag "PROJECT" project)
                 (const :tag "PROJECT - SESSION" project-session)
                 (const :tag "Don't rename" nil)))

(defcustom herdr-sync-tab-labels t
  "Rename each agent's tab to the agent's session name (its terminal title)."
  :type 'boolean)

(defcustom herdr-workspace-label-function #'identity
  "Function mapping a project name to the workspace label shown in Herdr.
For example (lambda (name) (string-remove-prefix \"ktzn-\" name))."
  :type 'function)

(defcustom herdr-agent-display 'tui
  "Where `herdr-switch-agent', `herdr-goto-blocked' and `herdr-new-agent'
show an agent.  `tui' focuses it inside the *herdr* TUI; `buffer' attaches
it directly in its own Ghostel buffer via `herdr agent attach', one buffer
per agent and no sidebar.  `herdr-attach-agent' always uses a buffer."
  :type '(choice (const :tag "Herdr TUI" tui)
                 (const :tag "One buffer per agent" buffer)))

(defcustom herdr-attach-takeover t
  "Pass --takeover to `herdr agent attach'.
Herdr allows one writable attach client per terminal; with this on,
attaching from Emacs replaces another client (e.g. a terminal), and
without it the attach is refused and the buffer closes at once."
  :type 'boolean)

(defun herdr--bind-menu-key (key bind)
  "Bind KEY to `herdr-menu' globally and in Ghostel, or unbind when BIND is nil."
  (when key
    (if bind
        (progn
          (keymap-global-set key #'herdr-menu)
          (unless (member key ghostel-keymap-exceptions)
            (setopt ghostel-keymap-exceptions
                    (append ghostel-keymap-exceptions (list key)))))
      (when (eq (keymap-global-lookup key) #'herdr-menu)
        (keymap-global-unset key))
      (when (member key ghostel-keymap-exceptions)
        (setopt ghostel-keymap-exceptions (remove key ghostel-keymap-exceptions))))))

(defcustom herdr-menu-repeat-command #'herdr-toggle
  "Command run by pressing `herdr-menu-key' again inside the menu.
`herdr-toggle' opens or hides the *herdr* TUI; `herdr-agents' opens the
agent list buffer."
  :type '(choice (const :tag "Open / hide Herdr TUI" herdr-toggle)
                 (const :tag "Open agent list" herdr-agents)
                 function))

(defcustom herdr-menu-key nil
  "Global key for `herdr-menu', a `key-valid-p' string such as \"C-9\".
Bound as soon as it is set (independently of `herdr-mode'), and also
added to `ghostel-keymap-exceptions' so it reaches Emacs from terminal
buffers.  Pressing it again inside the menu runs `herdr-menu-repeat-command'."
  :type '(choice (const :tag "None" nil) (string :tag "Key"))
  :set (lambda (sym val)
         (herdr--bind-menu-key (and (boundp sym) (symbol-value sym)) nil)
         (set-default sym val)
         (herdr--bind-menu-key val t)))

;;; Internal state

(defvar herdr--blocked-count 0)
(defvar herdr--poll-timer nil)
(defvar herdr--poll-in-flight nil)
(defvar herdr--poll-error-notified nil)

(defconst herdr--buffer-name "*herdr*")
(defconst herdr--agents-buffer-name "*herdr-agents*")

(defvar-local herdr--agents nil
  "Agents currently shown in the status buffer, sorted.")

(defvar-local herdr--attached-pane nil
  "Pane id this buffer is directly attached to, or nil.")

(defconst herdr--status-priority
  '(("blocked" . 0) ("done" . 1) ("working" . 2) ("idle" . 3) ("unknown" . 4)))

(defconst herdr--agent-kinds
  '("claude" "codex" "pi" "gemini" "cursor" "devin" "agy" "cline"
    "omp" "mastracode" "opencode" "copilot" "kimi" "kiro" "droid"
    "amp" "grok" "hermes" "kilo" "qodercli" "qwen" "maki")
  "Values accepted by `herdr agent start --kind'.")

;;; JSON helpers

(defun herdr--call-json-sync (&rest args)
  "Run herdr with ARGS synchronously, return parsed JSON or nil."
  (with-temp-buffer
    (let ((exit (apply #'call-process herdr-executable nil t nil args)))
      (when (zerop exit)
        (goto-char (point-min))
        (condition-case nil
            (json-parse-buffer :object-type 'alist :null-object nil)
          (json-parse-error nil))))))

(defun herdr--call-async (callback &rest args)
  "Run herdr with ARGS asynchronously, call CALLBACK with parsed JSON or nil."
  (let ((buf (generate-new-buffer " *herdr-async*")))
    (make-process
     :name "herdr-async"
     :buffer buf
     :command (cons herdr-executable args)
     :connection-type 'pipe
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (let ((result nil))
               (when (and (zerop (process-exit-status proc))
                          (buffer-live-p buf))
                 (with-current-buffer buf
                   (goto-char (point-min))
                   (condition-case nil
                       (setq result (json-parse-buffer :object-type 'alist
                                                       :null-object nil))
                     (json-parse-error nil))))
               (funcall callback result))
           (when (buffer-live-p buf)
             (kill-buffer buf))))))))

;;; Agent data

(defun herdr--extract-agents (json)
  "Agent alists from a `herdr api snapshot' JSON response."
  (when-let* ((snap (alist-get 'snapshot (alist-get 'result json))))
    (let ((ws-labels (mapcar (lambda (w)
                               (cons (alist-get 'workspace_id w) (alist-get 'label w)))
                             (alist-get 'workspaces snap)))
          (tab-labels (mapcar (lambda (tb)
                                (cons (alist-get 'tab_id tb) (alist-get 'label tb)))
                              (alist-get 'tabs snap))))
      (mapcar (lambda (a)
                (let ((ws (alist-get 'workspace_id a))
                      (tab (alist-get 'tab_id a)))
                  `((status . ,(or (alist-get 'agent_status a) "unknown"))
                    (name . ,(alist-get 'name a))
                    (kind . ,(alist-get 'agent a))
                    (target . ,(or (alist-get 'name a) (alist-get 'pane_id a)))
                    (session . ,(alist-get 'terminal_title_stripped a))
                    (cwd . ,(or (alist-get 'foreground_cwd a) (alist-get 'cwd a)))
                    (pane-id . ,(alist-get 'pane_id a))
                    (workspace-id . ,ws)
                    (workspace-label . ,(cdr (assoc ws ws-labels)))
                    (tab-id . ,tab)
                    (tab-label . ,(cdr (assoc tab tab-labels)))
                    (seq . ,(or (alist-get 'state_change_seq a) 0)))))
              (alist-get 'agents snap)))))

(defun herdr--workspace-for-root (root json)
  "Workspace id whose panes run in ROOT, preferring the focused one, or nil."
  (when-let* ((snap (alist-get 'snapshot (alist-get 'result json))))
    (let* ((root (file-truename (file-name-as-directory root)))
           (panes (seq-filter
                   (lambda (p)
                     (when-let* ((cwd (alist-get 'cwd p)))
                       (equal (file-truename (file-name-as-directory cwd)) root)))
                   (alist-get 'panes snap)))
           (focused (seq-find (lambda (p) (eq (alist-get 'focused p) t)) panes)))
      (alist-get 'workspace_id (or focused (car panes))))))

(defun herdr--fetch-agents-sync ()
  "Fetch agents from a live snapshot, syncing labels on the way."
  (let ((agents (herdr--extract-agents
                 (herdr--call-json-sync "api" "snapshot"))))
    (herdr--sync-labels agents)
    agents))

(defun herdr--sort-agents (agents)
  "Sort AGENTS by attention, then most recent state change first."
  (sort agents
        (lambda (a b)
          (let ((pa (or (cdr (assoc (alist-get 'status a) herdr--status-priority)) 5))
                (pb (or (cdr (assoc (alist-get 'status b) herdr--status-priority)) 5)))
            (if (= pa pb)
                (> (alist-get 'seq a) (alist-get 'seq b))
              (< pa pb))))))

(defun herdr--format-candidate (agent)
  "Format AGENT as \"[status] session (kind) · cwd · pane\"."
  (let* ((session (alist-get 'session agent))
         (who (or (and session (not (string-empty-p session)) session)
                  (alist-get 'name agent)
                  (alist-get 'pane-id agent)))
         (kind (alist-get 'kind agent))
         (cwd (alist-get 'cwd agent)))
    (string-join
     (delq nil (list (format "[%s] %s%s" (alist-get 'status agent) who
                             (if kind (format " (%s)" kind) ""))
                     (and cwd (abbreviate-file-name cwd))
                     (alist-get 'pane-id agent)))
     " · ")))

(defun herdr--pick-agent (&optional prompt)
  "Pick an agent via `completing-read', return its alist."
  (let* ((agents (herdr--sort-agents (herdr--fetch-agents-sync)))
         (candidates (mapcar (lambda (a) (cons (herdr--format-candidate a) a))
                             agents)))
    (unless candidates
      (user-error "No herdr agents"))
    (cdr (assoc (completing-read (or prompt "Agent: ") candidates nil t)
                candidates))))

;;; Workspace labels

(defun herdr--project-name (dir)
  "Workspace label for DIR: its project.el name (else directory name), filtered
through `herdr-workspace-label-function'."
  (funcall herdr-workspace-label-function
           (or (let ((default-directory dir))
                 (when-let* ((proj (project-current nil)))
                   (project-name proj)))
               (file-name-nondirectory (directory-file-name dir)))))

(defun herdr--generic-title-p (title kind)
  "Non-nil if TITLE is the agent's default title rather than a session name."
  (or (string-empty-p title)
      (string-equal-ignore-case title "claude code")
      (and kind (string-equal-ignore-case title kind))))

(defun herdr--workspace-label (agent)
  "Desired workspace label for AGENT, or nil.
Follows `herdr-sync-workspace-labels'."
  (when-let* ((cwd (alist-get 'cwd agent))
              (project (herdr--project-name cwd)))
    (let ((session (alist-get 'session agent)))
      (pcase herdr-sync-workspace-labels
        ('project project)
        ('project-session
         (if (and session (not (herdr--generic-title-p session (alist-get 'kind agent))))
             (format "%s - %s" project session)
           project))))))

(defun herdr--sync-labels (agents)
  "Sync workspace and tab labels for AGENTS as configured."
  (when herdr-sync-workspace-labels
    (herdr--sync-workspace-labels agents))
  (when herdr-sync-tab-labels
    (herdr--sync-tab-labels agents)))

(defun herdr--sync-tab-labels (agents)
  "Rename each agent's tab to its session name when it differs."
  (let (seen)
    (dolist (a agents)
      (let ((tab (alist-get 'tab-id a))
            (session (alist-get 'session a)))
        (when (and tab session (not (member tab seen))
                   (not (herdr--generic-title-p session (alist-get 'kind a))))
          (push tab seen)
          (unless (equal session (alist-get 'tab-label a))
            (herdr--call-async #'ignore "tab" "rename" tab session)))))))

(defun herdr--sync-workspace-labels (agents)
  "Rename each agent's workspace when its label differs from the desired one."
  (let (seen)
    (dolist (a agents)
      (let ((ws (alist-get 'workspace-id a)))
        (when (and ws (not (member ws seen)))
          (push ws seen)
          (when-let* ((label (herdr--workspace-label a)))
            (unless (equal label (alist-get 'workspace-label a))
              (herdr--call-async #'ignore "workspace" "rename" ws label))))))))

;;; Buffer management

(defun herdr--buffer-live-p ()
  "Return non-nil if the *herdr* buffer has a live ghostel process."
  (when-let* ((buf (get-buffer herdr--buffer-name)))
    (and (buffer-live-p buf)
         (buffer-local-value 'ghostel--process buf)
         (process-live-p (buffer-local-value 'ghostel--process buf)))))

(defun herdr--create-buffer ()
  "Create the *herdr* buffer with herdr running in ghostel."
  (let ((buf (generate-new-buffer herdr--buffer-name)))
    (with-current-buffer buf
      (ghostel-mode)
      ;; A side-by-side popup halves the PTY width; Herdr's reflow then
      ;; exposes rows that agents drew full-width as duplicated/merged lines.
      (setq-local split-width-threshold nil))
    (pop-to-buffer buf '((display-buffer-same-window)))
    (ghostel-exec buf herdr-executable nil '((kind . herdr)))
    buf))

;;;###autoload
(defun herdr ()
  "Pop to the *herdr* buffer, creating it if needed."
  (interactive)
  (let ((buf (get-buffer herdr--buffer-name)))
    (when (and buf (not (herdr--buffer-live-p)))
      (kill-buffer buf)
      (setq buf nil))
    (if buf
        (pop-to-buffer buf '((display-buffer-same-window)))
      (herdr--create-buffer))
    (herdr--ensure-timer)))

;;; Direct attach (one buffer per agent)

(defun herdr--attach-buffer-name (agent)
  "Buffer name for a direct attach to AGENT: \"*herdr: session*\"."
  (let ((session (alist-get 'session agent)))
    (format "*herdr: %s*"
            (or (and session (not (string-empty-p session)) session)
                (alist-get 'name agent)
                (alist-get 'pane-id agent)))))

(defun herdr--find-attach-buffer (pane-id)
  "Live buffer directly attached to PANE-ID, or nil.  Dead ones are killed."
  (seq-find (lambda (buf)
              (when (equal (buffer-local-value 'herdr--attached-pane buf) pane-id)
                (if (process-live-p (buffer-local-value 'ghostel--process buf))
                    t
                  (kill-buffer buf)
                  nil)))
            (buffer-list)))

(defun herdr--create-attach-buffer (agent)
  "Create a buffer directly attached to AGENT's pane and pop to it."
  (let* ((pane (alist-get 'pane-id agent))
         (args (append (list "agent" "attach" pane)
                       (and herdr-attach-takeover '("--takeover"))))
         (buf (generate-new-buffer (herdr--attach-buffer-name agent))))
    (with-current-buffer buf
      (ghostel-mode)
      (setq-local herdr--attached-pane pane
                  split-width-threshold nil))
    (pop-to-buffer buf '((display-buffer-same-window)))
    (ghostel-exec buf herdr-executable args
                  `((kind . herdr-attach)
                    (command . ,(cons herdr-executable args))))
    buf))

(defun herdr--attach-agent (agent &optional reattach)
  "Pop to the buffer directly attached to AGENT, creating it if needed.
With REATTACH, drop an existing buffer and attach afresh."
  (let ((buf (herdr--find-attach-buffer (alist-get 'pane-id agent))))
    (when (and buf reattach)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))
      (setq buf nil))
    (if buf
        (pop-to-buffer buf '((display-buffer-same-window)))
      (herdr--create-attach-buffer agent))))

(defun herdr--attach-buffers ()
  "Live attach buffers, most recently selected first."
  (seq-filter (lambda (buf)
                (and (buffer-local-value 'herdr--attached-pane buf)
                     (process-live-p (buffer-local-value 'ghostel--process buf))))
              (buffer-list)))

;;;###autoload
(defun herdr-last-agent ()
  "Pop to the most recently used agent buffer other than the current one."
  (interactive)
  (let ((buf (or (seq-find (lambda (b) (not (eq b (current-buffer))))
                           (herdr--attach-buffers))
                 (user-error "No other agent buffer"))))
    (pop-to-buffer buf)))

(defun herdr--show-agent (agent)
  "Show AGENT as configured by `herdr-agent-display'."
  (pcase herdr-agent-display
    ('buffer (herdr--attach-agent agent))
    (_ (herdr--call-json-sync "agent" "focus" (alist-get 'target agent))
       (herdr))))

;;; Commands

;;;###autoload
(defun herdr-hide ()
  "Hide the *herdr* window, restoring what it displaced."
  (interactive)
  (if-let* ((win (get-buffer-window herdr--buffer-name)))
      (quit-restore-window win 'bury)
    (user-error "*herdr* is not displayed")))

;;;###autoload
(defun herdr-toggle ()
  "Pop to *herdr*, or hide it when its window is already selected."
  (interactive)
  (if (eq (current-buffer) (get-buffer herdr--buffer-name))
      (herdr-hide)
    (herdr)))

;;;###autoload
(defun herdr-switch-agent ()
  "Pick an agent and show it, as configured by `herdr-agent-display'."
  (interactive)
  (herdr--show-agent (herdr--pick-agent "Switch to agent: ")))

;;;###autoload
(defun herdr-attach-agent (&optional reattach)
  "Pick an agent and attach to it directly in its own buffer, no sidebar.
With prefix REATTACH, replace an existing buffer with a fresh attach."
  (interactive "P")
  (herdr--attach-agent (herdr--pick-agent "Attach to agent: ") reattach))

;;;###autoload
(defun herdr-goto-blocked ()
  "Show the first blocked agent, as configured by `herdr-agent-display'."
  (interactive)
  (let ((blocked (seq-find (lambda (a) (equal (alist-get 'status a) "blocked"))
                           (herdr--sort-agents (herdr--fetch-agents-sync)))))
    (unless blocked
      (user-error "No blocked agents"))
    (herdr--show-agent blocked)))

(defun herdr--sanitize-name (str)
  "Sanitize STR to a valid herdr agent name."
  (let ((clean (replace-regexp-in-string "[^a-z0-9_-]" "-" (downcase str))))
    (setq clean (replace-regexp-in-string "^[^a-z]+" "" clean))
    (setq clean (replace-regexp-in-string "-+" "-" clean))
    (if (string-empty-p clean) "agent" clean)))

(defun herdr--unique-agent-name (base agents)
  "Return BASE or BASE-N not used by any of AGENTS, at most 32 chars."
  (let ((existing (delq nil (mapcar (lambda (a) (alist-get 'name a)) agents)))
        (name (truncate-string-to-width base 32))
        (n 2))
    (while (member name existing)
      (setq name (truncate-string-to-width (format "%s-%d" base n) 32)
            n (1+ n)))
    name))

;;;###autoload
(defun herdr-new-agent ()
  "Start a new agent for the current project.
Adds a tab to the project's existing Herdr workspace, or creates the
workspace when there is none."
  (interactive)
  (let* ((proj (project-current t))
         (root (expand-file-name (project-root proj)))
         (proj-name (herdr--project-name root))
         (kind (completing-read "Agent kind: " herdr--agent-kinds nil t
                                nil nil herdr-default-agent-kind))
         (json (herdr--call-json-sync "api" "snapshot"))
         (name (herdr--unique-agent-name
                (herdr--sanitize-name (concat proj-name "-" kind))
                (herdr--extract-agents json)))
         (ws (herdr--workspace-for-root root json))
         (created (alist-get 'result
                             (if ws
                                 (herdr--call-json-sync
                                  "tab" "create" "--workspace" ws
                                  "--cwd" root "--label" kind "--focus")
                               (herdr--call-json-sync
                                "workspace" "create"
                                "--cwd" root "--label" proj-name "--focus"))))
         (pane (alist-get 'pane_id (alist-get 'root_pane created))))
    (unless pane
      (user-error "Failed to create herdr %s" (if ws "tab" "workspace")))
    (herdr--call-async
     (lambda (result)
       (message "herdr: %s %s in %s"
                (if result "started" "FAILED to start") name pane)
       ;; `agent attach' needs a live agent in the pane, so attach only
       ;; once the start has succeeded.
       (when (and result (eq herdr-agent-display 'buffer))
         (herdr--attach-agent `((pane-id . ,pane) (name . ,name)))))
     "agent" "start" name "--kind" kind "--pane" pane)
    (unless (eq herdr-agent-display 'buffer)
      (herdr))))

;;; Send region

(defun herdr--detect-language ()
  "Derive a fence language name from `major-mode'."
  (let ((name (symbol-name major-mode)))
    (cond
     ((string-match "\\`\\(.+\\)-ts-mode\\'" name) (match-string 1 name))
     ((string-match "\\`\\(.+\\)-mode\\'" name) (match-string 1 name))
     (t name))))

(defun herdr--format-region (beg end)
  "Format region BEG..END as a fenced code block with metadata."
  (let* ((lang (herdr--detect-language))
         (file (buffer-file-name))
         (proj (project-current))
         (root (and proj (project-root proj)))
         (rel-path (if (and file root)
                       (file-relative-name file root)
                     (and file (file-name-nondirectory file))))
         (line-beg (line-number-at-pos beg t))
         (line-end (line-number-at-pos end t))
         (branch (and root (vc-git--symbolic-ref root)))
         (text (buffer-substring-no-properties beg end))
         (meta (delq nil (list (and root (format "project: %s" (abbreviate-file-name root)))
                               (and branch (format "branch: %s" branch))))))
    (concat "```" lang " " (or rel-path "") (format " L%d-L%d" line-beg line-end) "\n"
            text
            (unless (string-suffix-p "\n" text) "\n")
            "```\n"
            (when meta (concat "\n" (string-join meta "\n") "\n")))))

;;;###autoload
(defun herdr-send-region (beg end &optional submit)
  "Send region BEG..END to a Herdr agent.
Stages the text in the agent's prompt without submitting; with prefix
arg SUBMIT, submits it as a prompt (`herdr agent prompt')."
  (interactive "r\nP")
  (let* ((agent (herdr--pick-agent "Send to agent: "))
         (payload (herdr--format-region beg end))
         (ok (if submit
                 (herdr--call-json-sync "agent" "prompt"
                                             (alist-get 'target agent) payload)
               (herdr--call-json-sync "pane" "send-text"
                                           (alist-get 'pane-id agent) payload))))
    (unless ok
      (user-error "herdr refused the %s (agent blocked or gone?)"
                  (if submit "prompt" "text")))
    (herdr)))

;;; Blocked indicator

(defun herdr--mode-line-segment ()
  "Mode-line string showing the blocked agent count, empty when zero."
  (if (> herdr--blocked-count 0)
      (propertize (format " ● %d " herdr--blocked-count)
                  'face 'warning
                  'help-echo (format "%d blocked agent(s)" herdr--blocked-count))
    ""))

(defun herdr--poll ()
  "Refresh the blocked count (and workspace labels) from a snapshot."
  (when (and herdr--poll-timer (not herdr--poll-in-flight))
    (setq herdr--poll-in-flight t)
    (herdr--call-async
     (lambda (json)
       (setq herdr--poll-in-flight nil)
       (if (null json)
           (progn
             (setq herdr--blocked-count 0)
             (unless herdr--poll-error-notified
               (setq herdr--poll-error-notified t)
               (message "herdr: herdr not reachable, polling paused"))
             (herdr--stop-timer))
         (setq herdr--poll-error-notified nil)
         (let ((agents (herdr--extract-agents json)))
           (setq herdr--blocked-count
                 (seq-count (lambda (a) (equal (alist-get 'status a) "blocked")) agents))
           (herdr--sync-labels agents)
           (herdr--agents-render agents)))
       (force-mode-line-update t))
     "api" "snapshot")))

(defun herdr--ensure-timer ()
  "Start the poll timer if not running."
  (unless herdr--poll-timer
    (setq herdr--poll-error-notified nil
          herdr--poll-timer
          (run-with-timer 0 herdr-poll-interval #'herdr--poll))))

(defun herdr--stop-timer ()
  "Stop the poll timer."
  (when herdr--poll-timer
    (cancel-timer herdr--poll-timer)
    (setq herdr--poll-timer nil)))

;;; Status buffer

(defvar-keymap herdr-agents-mode-map
  :parent tabulated-list-mode-map
  "RET" #'herdr-agents-show
  "a" #'herdr-agents-attach
  "n" #'next-line
  "p" #'previous-line
  "s" #'herdr-agents-sort
  "c" #'herdr-new-agent
  "o" #'herdr)

(define-derived-mode herdr-agents-mode tabulated-list-mode "Herdr-Agents"
  "Live list of Herdr agents, refreshed by the `herdr-mode' poll.
RET shows the agent (see `herdr-agent-display'), `a' attaches it in a buffer,
`s' asks which column to sort by."
  (setq tabulated-list-format [("Status" 8 t)
                               ("Session" 60 t)
                               ("Kind" 7 t)
                               ("Project" 18 t)
                               ("Pane" 6 t)]
        tabulated-list-padding 1
        tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'herdr--agents-revert nil t)
  (tabulated-list-init-header))

(defun herdr--status-face (status)
  "Face for an agent STATUS string."
  (pcase status
    ("blocked" 'warning)
    ("done" 'success)
    ("working" 'default)
    (_ 'shadow)))

(defun herdr--agent-entry (agent)
  "Tabulated-list entry for AGENT, keyed by pane id."
  (let* ((status (alist-get 'status agent))
         (session (alist-get 'session agent))
         (cwd (alist-get 'cwd agent))
         (pane (alist-get 'pane-id agent)))
    (list pane
          (vector (propertize status 'face (herdr--status-face status))
                  (or (and session (not (string-empty-p session)) session)
                      (alist-get 'name agent)
                      pane)
                  (or (alist-get 'kind agent) "")
                  (if cwd (herdr--project-name cwd) "")
                  pane))))

(defun herdr--agents-set-entries (agents)
  "Store AGENTS sorted and rebuild `tabulated-list-entries' from them."
  (setq herdr--agents (herdr--sort-agents agents)
        tabulated-list-entries (mapcar #'herdr--agent-entry herdr--agents)))

(defun herdr--agents-render (agents)
  "Redraw the status buffer with AGENTS when it exists, keeping point."
  (when-let* ((buf (get-buffer herdr--agents-buffer-name)))
    (with-current-buffer buf
      (when (derived-mode-p 'herdr-agents-mode)
        (herdr--agents-set-entries agents)
        (tabulated-list-print t)))))

(defun herdr--agents-revert ()
  "Refill the status buffer from a fresh snapshot (for `revert-buffer')."
  (herdr--agents-set-entries (herdr--fetch-agents-sync)))

(defun herdr--agent-at-point ()
  "Agent alist for the status-buffer row at point."
  (let ((pane (tabulated-list-get-id)))
    (or (and pane (seq-find (lambda (a) (equal (alist-get 'pane-id a) pane))
                            herdr--agents))
        (user-error "No agent on this line"))))

(defconst herdr--attention-sort-label "attention (default)"
  "Sort choice that restores the blocked-first order.")

(defun herdr-agents-sort ()
  "Pick a column to sort by; picking the current one reverses the order.
The attention entry restores the default blocked-first order."
  (interactive)
  (let* ((columns (mapcar #'car tabulated-list-format))
         (choice (completing-read "Sort by: "
                                  (cons herdr--attention-sort-label columns)
                                  nil t))
         (current tabulated-list-sort-key))
    (setq tabulated-list-sort-key
          (cond ((equal choice herdr--attention-sort-label) nil)
                ((equal choice (car current)) (cons choice (not (cdr current))))
                (t (cons choice nil))))
    (tabulated-list-init-header)
    ;; `tabulated-list-print' sorts the entries list destructively, so
    ;; rebuild it before printing in the new order.
    (herdr--agents-revert)
    (tabulated-list-print t)))

(defun herdr-agents-show (&optional reattach)
  "Show the agent at point in this window, per `herdr-agent-display'.
With prefix REATTACH, a buffer-mode agent is attached afresh."
  (interactive "P")
  (if (and reattach (eq herdr-agent-display 'buffer))
      (herdr--attach-agent (herdr--agent-at-point) t)
    (herdr--show-agent (herdr--agent-at-point))))

(defun herdr-agents-attach (&optional reattach)
  "Attach to the agent at point in its own buffer, in this window.
With prefix REATTACH, replace an existing buffer with a fresh attach."
  (interactive "P")
  (herdr--attach-agent (herdr--agent-at-point) reattach))

;;;###autoload
(defun herdr-agents ()
  "Pop to the live agent list; it refreshes with the `herdr-mode' poll."
  (interactive)
  (let ((buf (get-buffer-create herdr--agents-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'herdr-agents-mode)
        (herdr-agents-mode))
      (herdr--agents-revert)
      (tabulated-list-print t))
    (pop-to-buffer buf)
    (herdr--ensure-timer)))

;;; Transient menu

(defvar herdr--menu-repeat-key nil
  "Key that opens the *herdr* buffer from inside `herdr-menu'.")

(defun herdr--invoking-key ()
  "The key that invoked the current command as a `key-valid-p' string, or nil."
  (let ((ev last-command-event))
    (when (integerp ev)
      (let ((desc (key-description (vector ev))))
        (unless (member desc '("RET" "TAB" "ESC" "C-j" "DEL" "C-g"))
          desc)))))

;;;###autoload
(defun herdr-submit-region (beg end)
  "Send region BEG..END to an agent and submit it."
  (interactive "r")
  (herdr-send-region beg end t))

(defun herdr--menu-description ()
  "Menu heading with the live blocked count."
  (if (> herdr--blocked-count 0)
      (format "Herdr  %s" (propertize (format "● %d blocked" herdr--blocked-count)
                                      'face 'warning))
    "Herdr"))

(defun herdr--repeat-description ()
  "Menu label for `herdr-menu-repeat-command'."
  (pcase herdr-menu-repeat-command
    ('herdr-toggle "Open / hide Herdr")
    ('herdr-agents "Open agent list")
    (cmd (format "%s" cmd))))

(defun herdr--menu-children (_)
  "Build menu suffixes; the repeat key wins over static keys."
  (let* ((k herdr--menu-repeat-key)
         (specs `(,@(and k `((,k ,(herdr--repeat-description)
                              ,herdr-menu-repeat-command)))
                  ("o" "Open Herdr" herdr)
                  ("q" "Hide Herdr" herdr-hide)
                  ("s" "Switch agent" herdr-switch-agent)
                  ("a" "Attach agent in buffer" herdr-attach-agent)
                  ("SPC" "Last agent buffer" herdr-last-agent)
                  ("n" "New agent for project" herdr-new-agent)
                  ("b" herdr-goto-blocked
                   :description ,(lambda () (format "Blocked (%d)" herdr--blocked-count)))
                  ("r" "Send region (stage)" herdr-send-region :if use-region-p)
                  ("R" "Send region & submit" herdr-submit-region :if use-region-p)
                  ("m" herdr-mode
                   :description ,(lambda ()
                                   (format "Poll & mode-line (%s)"
                                           (if herdr-mode "on" "off"))))))
         (seen nil)
         (unique (seq-filter (lambda (s)
                               (unless (member (car s) seen)
                                 (push (car s) seen)))
                             specs)))
    (transient-parse-suffixes 'herdr-menu unique)))

;;;###autoload (autoload 'herdr-menu "herdr" nil t)
(transient-define-prefix herdr-menu ()
  "Herdr commands.  Pressing `herdr-menu-key' again runs
`herdr-menu-repeat-command'."
  [:description herdr--menu-description
   :setup-children herdr--menu-children]
  (interactive)
  (setq herdr--menu-repeat-key
        (or herdr-menu-key (herdr--invoking-key)))
  (transient-setup 'herdr-menu))

;;; Global minor mode

(defvar herdr--mode-line-string '(:eval (herdr--mode-line-segment)))

;;;###autoload
(define-minor-mode herdr-mode
  "Global minor mode owning the Herdr poll timer and mode-line segment."
  :global t
  :lighter nil
  (if herdr-mode
      (progn
        (unless (member herdr--mode-line-string global-mode-string)
          (push herdr--mode-line-string global-mode-string))
        (herdr--ensure-timer))
    (herdr--stop-timer)
    (setq herdr--blocked-count 0
          global-mode-string (delete herdr--mode-line-string global-mode-string))
    (force-mode-line-update t)))

(provide 'herdr)
;;; herdr.el ends here
