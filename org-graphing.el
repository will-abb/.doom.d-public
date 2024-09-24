;;; org-graphing.el -*- lexical-binding: t; -*-

(defun split-string-by-word-limit (string word-limit)
  "Split STRING into substrings of WORD-LIMIT words."
  (let ((words (split-string string " ")))
    (mapconcat 'identity
               (cl-loop for i from 0 to (1- (/ (length words) word-limit))
                        collect (string-join (seq-take (nthcdr (* i word-limit) words) word-limit) " "))
               "\n")))

(defun extract-outline-content ()
  "Extract the Org-mode outline content. If a region is selected, use that; otherwise, use the entire buffer."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun parse-outline-headline (headline)
  "Parse the HEADLINE for priority and status, returning cleaned headline, color, and style."
  (let* ((priority (if (string-match "\\[#\\([A-C]\\)\\]" headline)
                       (match-string 1 headline)
                     nil))
         (status (if (string-match "^\\(TODO\\|DONE\\) " headline)
                     (match-string 1 headline)
                   nil))
         (cleaned-headline (replace-regexp-in-string "\\(\\[#[A-C]\\]\\|\\(TODO\\|DONE\\) \\)" "" headline))
         (color (cond
                 ((equal status "DONE") "darkgrey")
                 ((equal priority "A") "lightcoral")
                 ((equal priority "B") "lightgoldenrod")
                 ((equal priority "C") "lightyellow")
                 (t "white")))
         (style (if (equal status "DONE") "style=filled, fillcolor=darkgrey" (format "style=filled, fillcolor=%s" color))))
    (list cleaned-headline color style)))

(defun generate-dot-nodes-edges (outline word-limit)
  "Generate DOT nodes and edges from the OUTLINE. Split headlines by WORD-LIMIT."
  (let ((dot-nodes '())
        (dot-edges '())
        (parent-stack '())
        (current-node 0))
    (with-temp-buffer
      (insert outline)
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) \\(.*\\)" nil t)
        (let* ((level (length (match-string 1)))
               (headline (match-string 2))
               (parsed-headline (parse-outline-headline headline))
               (cleaned-headline (nth 0 parsed-headline))
               (color (nth 1 parsed-headline))
               (style (nth 2 parsed-headline)))
          (setq current-node (1+ current-node))
          (push (format "node_%d [label=\"%s\", shape=box, %s];"
                        current-node
                        (split-string-by-word-limit cleaned-headline word-limit)
                        style)
                dot-nodes)
          (while (> (length parent-stack) level)
            (pop parent-stack))
          (when parent-stack
            (push (format "node_%d -> node_%d;" (car parent-stack) current-node) dot-edges))
          (push current-node parent-stack))))
    (list dot-nodes dot-edges)))

(defun format-dot-source-block (dot-nodes dot-edges)
  "Format DOT-NODES and DOT-EDGES into a DOT source block."
  (concat
   "#+BEGIN_SRC dot :file output.png :cmdline -Kdot -Tpng\n"
   "digraph Org {\n"
   "  node [fontname=\"Helvetica, Arial, sans-serif\"];\n"
   "  graph [splines=ortho];\n" ;; Use orthogonal edges for a cleaner layout
   (mapconcat 'identity (reverse dot-nodes) "\n")
   "\n"
   (mapconcat 'identity (reverse dot-edges) "\n")
   "\n}\n"
   "#+END_SRC\n"))

(defun org-outline-to-dot-src-block ()
  "Convert the highlighted Org-mode outline to a Graphviz DOT source block.
If no region is selected, use the entire buffer. Add colors based on priority and style based on TODO/DONE status."
  (interactive)
  (let* ((outline (extract-outline-content))
         (word-limit 3) ;; Change this value to adjust the word limit per line
         (nodes-edges (generate-dot-nodes-edges outline word-limit))
         (dot-nodes (nth 0 nodes-edges))
         (dot-edges (nth 1 nodes-edges))
         (dot-src (format-dot-source-block dot-nodes dot-edges)))
    (kill-new dot-src)
    (message "DOT source block copied to clipboard")
    dot-src))

(defun org-outline-to-image-buffer ()
  "Convert the Org-mode outline to a Graphviz image and display it in a new Org buffer.
If no region is selected, use the entire buffer."
  (interactive)
  (let* ((dot-src (org-outline-to-dot-src-block))
         (dot-file (make-temp-file "org-outline" nil ".dot"))
         (img-file (make-temp-file "org-outline" nil ".png"))
         (org-buffer (get-buffer-create "*Org Outline Image*")))
    ;; Write the DOT source to a temporary DOT file
    (with-temp-file dot-file
      (insert dot-src))
    ;; Generate the image using Graphviz
    (call-process "dot" nil nil nil "-Tpng" dot-file "-o" img-file)
    ;; Display the image in a new Org buffer
    (with-current-buffer org-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "[[file:%s]]" img-file))
      (org-display-inline-images)
      (read-only-mode 1))
    ;; Split the window and display the buffer
    (split-window-right)
    (other-window 1)
    (switch-to-buffer org-buffer)))

(defun org-outline-to-maximized-image-buffer ()
  "Convert the Org-mode outline to a Graphviz image and display it maximized in a new Org buffer.
If no region is selected, use the entire buffer."
  (interactive)
  (let* ((dot-src (org-outline-to-dot-src-block))
         (dot-file (make-temp-file "org-outline" nil ".dot"))
         (img-file (make-temp-file "org-outline" nil ".png"))
         (org-buffer (get-buffer-create "*Org Outline Image*")))
    ;; Write the DOT source to a temporary DOT file
    (with-temp-file dot-file
      (insert dot-src))
    ;; Generate the image using Graphviz
    (call-process "dot" nil nil nil "-Tpng" dot-file "-o" img-file)
    ;; Display the image in a new Org buffer
    (with-current-buffer org-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "[[file:%s]]" img-file))
      (org-display-inline-images)
      (read-only-mode 1))
    ;; Maximize the buffer and display it
    (delete-other-windows)
    (switch-to-buffer org-buffer)))

(provide 'org-graphing)
