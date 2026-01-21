;; -*- lexical-binding: t; -*-
(defconst atproto-collection-nsid "pub.leaflet.document")
(defconst atproto-user-did "did:plc:gijpvbkdbr56kazbdjhfvb3d")
(defconst atproto-pds "https://porcini.us-east.host.bsky.network")
(defconst max-entries 5)

(defun search-leaflet-fallback-block ()
  )

(defun at-datetime->timestamp (dt)
  (date-to-time dt))

(defun timestamp->humanreadable-string (ts)
  (format-time-string "%d/%m/%Y, %H:%M" ts))

(defun build-leaflet-html-entry (title description date date-humanreadable url)
  (format "<div class=\"entry\">\n<a class=\"base-anchor\" href=\"https://amybunny.leaflet.pub/%s\"></h3 class=\"title\">%s</h3></a>\n<p class=\"description\">%s</p>\n<div class=\"metadata\">\n<time datetime=%S class=\"published-at\">%s</time>\n</div>\n</div>" url title description date date-humanreadable))

(defun fetch-latest-entries ()
  (let ((url-request-method "GET")
	(endpoint (concat atproto-pds "/xrpc/com.atproto.repo.listRecords?repo=" atproto-user-did "&collection=" atproto-collection-nsid "&limit=" (number-to-string max-entries))))
    (with-current-buffer (url-retrieve-synchronously endpoint)
      (goto-char 0)
  (search-forward "\n\n")
  (gethash "records" (json-parse-string (buffer-substring (point) (point-max)))))))

(defun build-elements-from-entries (seq)
  (mapcar (lambda (rec)
	    (let* ((document-info (gethash "value" rec))
		   (url (car (last (string-split (gethash "uri" rec) "\/+")))))
	      (build-leaflet-html-entry
	       (gethash "title" document-info)
	       (gethash "description" document-info)
	       (gethash "publishedAt" document-info)
	       (timestamp->humanreadable-string (at-datetime->timestamp (gethash "publishedAt" document-info)))
	       url)))
	  seq))

(with-current-buffer "index.html"
  (save-excursion
    (goto-char 0)
    (let ((block-start (search-forward "<noscript>" nil t))
	  (block-end (- (search-forward "</noscript>" nil t) 11)))
      (delete-region block-start block-end)
      (goto-char (- (point) 11))
      (insert "\n")
      (dolist (element (build-elements-from-entries (fetch-latest-entries)))
	(insert (concat element "\n")))
      (indent-region block-start (search-forward "</noscript>" nil t)))))

