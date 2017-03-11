(import (rnrs)
	(text json object-builder)
	(text json)
	(srfi :64 testing))

(test-begin "JSON object")

(define-record-type location
  (fields precision latitude longitude address city state zip country))

(test-assert (json:builder? (json-object-builder
			     (@ list
				(make-location
				 "precision"
				 "Latitude"
				 "Longitude"
				 "Address"
				 "City"
				 "State"
				 "Zip"
				 "Country")))))

(let* ((json-string "[
 {
 \"precision\": \"zip\",
 \"Latitude\":  37.7668,
 \"Longitude\": -122.3959,
 \"Address\":   \"\",
 \"City\":      \"SAN FRANCISCO\",
 \"State\":     \"CA\",
 \"Zip\":       \"94107\",
 \"Country\":   \"US\"
 },
 {
 \"precision\": \"zip\",
 \"Latitude\":  37.371991,
 \"Longitude\": -122.026020,
 \"City\":      \"SUNNYVALE\",
 \"State\":     \"CA\",
 \"Zip\":       \"94085\",
 \"Country\":   \"US\"
 }
]")
       (builder (json-object-builder
		 (@ list
		    (make-location
		     "precision"
		     "Latitude"
		     "Longitude"
		     (? "Address" #f)
		     "City"
		     "State"
		     "Zip"
		     "Country"))))
       (serializer (json-object-serializer
		    (-> (("precision" location-precision)
			 ("Latitude" location-latitude)
			 ("Longitude" location-longitude)
			 (? "Address" #f location-address)
			 ("City" location-city)
			 ("State" location-state)
			 ("Zip" location-zip)
			 ("Country" location-country)))))
       (v (json-string->object json-string builder)))
  (define-syntax check-location
    (syntax-rules ()
      ((_ loc precision latitude longitude address city state zip country)
       (let ((loc1 loc))
	 (test-equal precision (location-precision loc1))
	 (test-equal latitude (location-latitude loc1))
	 (test-equal longitude (location-longitude loc1))
	 (test-equal address (location-address loc1))
	 (test-equal city (location-city loc1))
	 (test-equal state (location-state loc1))
	 (test-equal zip (location-zip loc1))
	 (test-equal country (location-country loc1))))))
  (test-assert (list? v))
  (test-assert (for-all location? v))
  (test-equal 2 (length v))
  (check-location (car v) "zip" 37.7668 -122.3959
		  "" "SAN FRANCISCO" "CA" "94107" "US")
  (check-location (cadr v)
		  "zip" 37.371991 -122.026020 #f "SUNNYVALE"
		  "CA" "94085" "US")

  (test-assert (json:serializer? serializer))
  (let ((s (object->json-string v serializer)))
    (test-assert (string? s))
    (test-equal (json-read (open-string-input-port json-string))
		(json-read (open-string-input-port s)))))

(define-record-type image-holder
  (fields image))
(define-record-type image
  (fields width height title thumbnail animated ids))
(define-record-type thumbnail
  (fields url height width))

(let* ((json-string "{
  \"Image\": {
    \"Width\":  800,
    \"Height\": 600,
    \"Title\":  \"View from 15th Floor\",
    \"Thumbnail\": {
      \"Url\":    \"http://www.example.com/image/481989943\",
      \"Height\": 125,
      \"Width\":  100
  },
    \"Animated\" : false,
    \"IDs\": [116, 943, 234, 38793]
  }
}"
)
       (builder (json-object-builder
		 (make-image-holder
		  ("Image"
		  (make-image
		   "Width"
		   "Height"
		   "Title"
		   ("Thumbnail"
		    (make-thumbnail
		     "Url"
		     "Height"
		     "Width"))
		   "Animated"
		   ("IDs" (@ list)))))))
       (serializer (json-object-serializer
		    (("Image" image-holder-image
		      (("Width" image-width)
		       ("Height" image-height)
		       ("Title" image-title)
		       ("Thumbnail" image-thumbnail
			(("Url" thumbnail-url)
			 ("Height" thumbnail-height)
			 ("Width" thumbnail-width)))
		       ("Animated" image-animated)
		       ("IDs" image-ids (->)))))))
       (v (json-string->object json-string builder)))
  (test-assert (image-holder? v))
  (let ((image (image-holder-image v)))
    (test-assert (image? image))
    (test-equal '(116 943 234 38793) (image-ids image))
    (let ((thumbnail (image-thumbnail image)))
      (test-assert (thumbnail? thumbnail))
      (test-equal "http://www.example.com/image/481989943"
		  (thumbnail-url thumbnail))))

  (let ((s (object->json-string v serializer)))
    (test-equal (json-read (open-string-input-port json-string))
		(json-read (open-string-input-port s)))))

;; other tests
(let ()
  (define-record-type foo
    (fields bar))
  (define serializer
    (json-object-serializer
     (("bar" foo-bar (@)))))
  (test-equal "{\"bar\": [1, 2, 3]}"
	      (object->json-string (make-foo '#(1 2 3)) serializer)))

(let ()
  (define-record-type foo
    (fields bar))
  (define serializer
    (json-object-serializer
     (("bar" foo-bar (@ bytevector-u8-ref bytevector-length)))))
  (test-equal "{\"bar\": [1, 2, 3]}"
	      (object->json-string (make-foo '#vu8(1 2 3)) serializer)))


(let ((json-string "{\"bar\": {\"buz\": 1}}"))
  (define-record-type foo
    (fields bar))
  (define-record-type bar
    (fields buz))
  (define bar-serializer
    (json-object-serializer
     (("buz" bar-buz))))
  (define serializer
    (json-object-serializer
     (("bar" foo-bar bar-serializer))))

  (define bar-builder (json-object-builder (make-bar "buz")))
  (define builder (json-object-builder (make-foo ("bar" bar-builder))))

  (test-equal json-string
	      (object->json-string
	       (json-string->object json-string builder)
	       serializer)))

(test-end)
