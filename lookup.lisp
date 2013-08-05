(in-package :cl-smaw)

(defun track-lookup-parser (track-lookup-object)
  (list :name (gethash "name" track-lookup-object)
	:available (gethash "available" track-lookup-object)
	:album (let* ((album-key (gethash "album" track-lookup-object)))
		 (list :name (gethash "name" album-key)
		       :released (gethash "released" album-key)
		       :href (gethash "href" album-key)))
	:popularity (gethash "popularity" track-lookup-object)
	:external-ids (mapcar #'(lambda (each) (list
						:type (gethash "type" each)
						:id (gethash "id" each)))
			      (gethash "external-ids" track-lookup-object))
	:length (gethash "length" track-lookup-object)
	:href (gethash "href" track-lookup-object)
	:artists (mapcar #'(lambda (each) (list
					   :href (gethash "href" each)
					   :name (gethash "name" each)))
			 (gethash "artists" track-lookup-object))
	:availability (list :territories (gethash "territories"
						  (gethash "availability" track-lookup-object)))
	:track-number (gethash "track-number" track-lookup-object)))
