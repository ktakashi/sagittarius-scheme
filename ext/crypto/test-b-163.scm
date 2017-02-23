;; [B-163,SHA-1]
#|
  Msg = 86752230200fc292fcb89597605c9ce117397779
  d = 13486dc5ca0ba84956d2f6dc43df0415656f0eac5
  Qx = 71765ccb031969d7332cc53890ee209520fb8ceab
  Qy = 2e99b4c30d3de389735cbeebb6e73ce9f67dc5412
  k = 17cdf80f62e42b21349a55a62591436363ec43c59
  R = 2ddace85a086746d8a4691ca61765719fbb69d928
  S = 17a9d0c14ff04cb6ae6d72d26701e5f69c5320e6b
|#
(test-ecdsa NIST-B-163 no-20
            #x86752230200fc292fcb89597605c9ce117397779
            #x13486dc5ca0ba84956d2f6dc43df0415656f0eac5
            #x71765ccb031969d7332cc53890ee209520fb8ceab
            #x2e99b4c30d3de389735cbeebb6e73ce9f67dc5412
            #x17cdf80f62e42b21349a55a62591436363ec43c59
            #x2ddace85a086746d8a4691ca61765719fbb69d928
            #x17a9d0c14ff04cb6ae6d72d26701e5f69c5320e6b
)

;; [B-163,SHA-224]
#|
  Msg = c6e9f645884a30ec4a4e036b4a4c1e08172706ebddd175400080eb83
  d = 2195f750f10b89ba5c4fbefd0fe76a3213b5fc96a
  Qx = 48fa6845e9b35b43c832f782fc729495a92014f8f
  Qy = 4a967b26229bca6a234344f63eb4272f83254e45e
  k = 379eb8961d496aaba2b732d1215b3cfc62773da3f
  R = 029ccd4e3a372a2cf1a96f205583e5b85ec762f74
  S = 304d311f55a40fed97677d56634a1615c56db96be
|#
(test-ecdsa NIST-B-163 no-28
            #xc6e9f645884a30ec4a4e036b4a4c1e08172706ebddd175400080eb83
            #x2195f750f10b89ba5c4fbefd0fe76a3213b5fc96a
            #x48fa6845e9b35b43c832f782fc729495a92014f8f
            #x4a967b26229bca6a234344f63eb4272f83254e45e
            #x379eb8961d496aaba2b732d1215b3cfc62773da3f
            #x029ccd4e3a372a2cf1a96f205583e5b85ec762f74
            #x304d311f55a40fed97677d56634a1615c56db96be
)

;; [B-163,SHA-256]
#|
  Msg = 29bf9800a1d491700fcc66640b0e9e54ee12dc05456dfad5d5726bf5d4aeb605
  d = 09b9bcd6a80841c1163dd1ea86fd86a1ee7399942
  Qx = 35ef74e1627d99fe9aee967e5f2cd687c77cbdf3b
  Qy = 435d36c7daba700a2030ab517eee552a1eab5ea30
  k = 2af314a74ae776920a1dff218ec40ece8f6ca91fb
  R = 3b2615f9da8e6fd395c2f0c599a0fb661b298a232
  S = 0c7aaa87ebdea0f90d47768cc43cee0608e0819a7
|#
(test-ecdsa NIST-B-163 no-32
            #x29bf9800a1d491700fcc66640b0e9e54ee12dc05456dfad5d5726bf5d4aeb605
            #x09b9bcd6a80841c1163dd1ea86fd86a1ee7399942
            #x35ef74e1627d99fe9aee967e5f2cd687c77cbdf3b
            #x435d36c7daba700a2030ab517eee552a1eab5ea30
            #x2af314a74ae776920a1dff218ec40ece8f6ca91fb
            #x3b2615f9da8e6fd395c2f0c599a0fb661b298a232
            #x0c7aaa87ebdea0f90d47768cc43cee0608e0819a7
)

;; [B-163,SHA-384]
#|
  Msg = 14b5a3cedb0364638b045e87adf5d324e658b528a0384473fb742cea62298aa5160974fee9e783b118f06e4599f20b16
  d = 02526cdb99e6dfa06045bba0648b2282e0a9a5af1
  Qx = 75224b91ef03cd8071fd777080e2c30380f941777
  Qy = 777f007985f627c91259560964585a6c1fb1513cc
  k = 0e46ed9f90c268a7ce04273376390d9af3e787218
  R = 1c8fe41a22eb05873a577332cca66130d57904836
  S = 043e74e4f4cf2b890693ab5c8da788dbfbb9b8621
|#
(test-ecdsa NIST-B-163 no-48
            #x14b5a3cedb0364638b045e87adf5d324e658b528a0384473fb742cea62298aa5160974fee9e783b118f06e4599f20b16
            #x02526cdb99e6dfa06045bba0648b2282e0a9a5af1
            #x75224b91ef03cd8071fd777080e2c30380f941777
            #x777f007985f627c91259560964585a6c1fb1513cc
            #x0e46ed9f90c268a7ce04273376390d9af3e787218
            #x1c8fe41a22eb05873a577332cca66130d57904836
            #x043e74e4f4cf2b890693ab5c8da788dbfbb9b8621
)

;; [B-163,SHA-512]
#|
  Msg = 42bd852de94dfb20fa365626b15ed4560a88535c3a5035e6078e42e1b18f9119d11f99f61bb1ce55a30ab0a23a6da6df876abb400b1581990d6415c04e3ddec2
  d = 041bdf12d45017fe825a30247ad18a2884e44cb59
  Qx = 7361b76f8060c1c5d05d46ade8fd82ff887861c24
  Qy = 0b7e43da4aca8dc5ef4bd501f09496cec335f4736
  k = 313f6b8eff840039c1d3f73b03c3456aefcc1727a
  R = 1db9b939df8762b5af3f592c929c1a795ee2fa193
  S = 2b72d2c570d95ac68dba5de51cca21beaea9ad200
|#
(test-ecdsa NIST-B-163 no-64
            #x42bd852de94dfb20fa365626b15ed4560a88535c3a5035e6078e42e1b18f9119d11f99f61bb1ce55a30ab0a23a6da6df876abb400b1581990d6415c04e3ddec2
            #x041bdf12d45017fe825a30247ad18a2884e44cb59
            #x7361b76f8060c1c5d05d46ade8fd82ff887861c24
            #x0b7e43da4aca8dc5ef4bd501f09496cec335f4736
            #x313f6b8eff840039c1d3f73b03c3456aefcc1727a
            #x1db9b939df8762b5af3f592c929c1a795ee2fa193
            #x2b72d2c570d95ac68dba5de51cca21beaea9ad200
)
