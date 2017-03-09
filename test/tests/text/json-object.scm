(import (rnrs)
	(text json object-builder)
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

(define json-string "[
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

(let* ((builder (json-object-builder
		 (@ list
		    (make-location
		     "precision"
		     "Latitude"
		     "Longitude"
		     (? "Address")
		     "City"
		     "State"
		     "Zip"
		     "Country"))))
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
		  "CA" "94085" "US"))



(test-end)
