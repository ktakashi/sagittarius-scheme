(test-ecdh-jwk
  "ecdh_webcrypto_test"
  :algorithm
  "ECDH"
  :curve
  "P-256"
  :encoding
  "webcrypto"
  :tests
  '(#(1
      "normal case"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "YtW9M3Kvdf6FoEBxXQ9QJCjgcEaGiwv9-mHXMa_kTyY")
        ("y"
         .
         "rDM6k6nnCoHNWpW1v40TmQ63QcjDiHK0oH0nWgFOMM8"))
      #(("crv" . "P-256")
        ("d"
         .
         "BhJGXImgI6sXhVsKa86_0_67U674QThke1NS4CwQw0Y")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "tZzHZx3Wprg24s2Tlu9WGLL_PoGS3XydNsJ8tW_5FmE")
        ("y"
         .
         "SCbZ29WuZM3YV1Bou8nmPyMepX7QMkiETAkzG5U5IFM"))
      #vu8(83 2 13 144 139 2 25 50 139 101 139 82 95 38 120 14 58 225 43 205 149 43 178 90 147 188 8 149 225 113 66 133)
      #t
      ())
    #(2
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WP1BaKh3lWA-KwQ5AoW9ym5X3mAn_iEd2dJeIhLSnmI")
        ("y"
         .
         "CA02vSJNdAVQkpXu0CoXFQ4DsxT5baN0RbDR0pN30Sw"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(3
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "D20gwEJh7MPpKEasrUjcjsXuNa4Ig_DS6nEhaQbuHEc")
        ("y"
         .
         "wEJomplt0SgwrkWTgulKrFa3F68uIIAhX55BlJsfUr4"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(4
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AMfe_rGhYjZzjpoRI7piG8jpo_JIWz-P_ef5zpj1qKE")
        ("y"
         .
         "yzOMORKxeS9gwrBuxSMeLYSw5Zbpt21BnOEF7ON5Hbw"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1)
      #t
      ())
    #(5
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6bmPssCsBF-MdhJf_ZnrilFXvh19s-hdZV7B2CECiM8")
        ("y"
         .
         "IY3yT9LCdGvlnfQSYu86l9mGdEsoNnSKdIYjCjGf_sA"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 1 0 0 0 0)
      #t
      ())
    #(6
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6UhOWPMzG2b_7W2Qyxx4Bl-ijPulx91DUgE9MlLuQnc")
        ("y"
         .
         "vXUDsEWji0skezLFlZNYDznmq_o3bD3KIM9_nPtlnhM"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 3 255 255 255 0 0 0 63 255 255 240 0 0 3 255 255 255 0 0 0 63 255 255 240 0 0 3 255 255 255)
      #t
      ())
    #(7
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dn1_u4Sqak2xB5NyZE5C7LL-wgDBeIIjksuLlQ_90Mk")
        ("y"
         .
         "HIaFPK_Qm1K6Lyh_DrqiZBWjz6uvksamF6GZiFY9neo"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 1 0 1)
      #t
      ())
    #(8
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "x01Uby_MbdOS-F5b4WfjWN6Qh1awwLsBy2nYZMoIPhw")
        ("y"
         .
         "k_lZ7s5uEO4RvTk0IH1lriivaLCSWFoVCSYOzrObku8"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(8 94 197 164 175 64 23 107 99 24 144 105 174 255 203 34 156 150 211 224 70 224 40 62 210 249 218 194 27 21 173 60)
      #t
      ())
    #(9
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "NPyfHnoJTNKVmNGEH6lhPb6CMT1jOlHWP7bv8HTMm5o")
        ("y"
         .
         "Ts_Z8ljFxNQhC0l1EhOiTFlpgr0dVOBEVEPyHvFUkqU"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(25 12 37 248 138 217 174 58 9 142 108 255 230 253 11 27 234 66 17 78 176 206 221 88 104 164 92 95 226 119 223 243)
      #t
      ())
    #(10
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "1clu_RkH_UjeKtcVrPgurlxmkP4-_hanjWHGjTv9EN8")
        ("y"
         .
         "A-rIFrnnt3YZKj9QdYh8DiJWF1BYM8qZfNoy_Q9nPF4"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(80 116 66 0 115 34 170 137 83 64 203 164 171 194 215 48 191 208 177 108 44 121 164 104 21 248 120 13 44 85 162 221)
      #t
      ())
    #(11
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "9HX1A6dw33LEWu3-QsAI9ZqlfnKyMvJmAL3QNTlXyyA")
        ("y"
         .
         "vbj2QFtJGAUKNUn0TAeo66ggzc5OzmmYiMY432b1T3w"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(95 23 123 254 25 186 170 238 89 126 104 182 168 122 81 158 128 94 157 40 167 12 183 47 212 15 15 229 167 84 186 69)
      #t
      ())
    #(12
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "88tnVLfiqG0GTfufkDGFqqTJK0gcLBof8nYwO7xBg-Q")
        ("y"
         .
         "nDGFmbCYTDVj3zOTEf4UOn2SHudbdVpSxvgE-Je4Cfc"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(127 255 0 1 255 252 0 7 255 240 0 31 255 192 0 127 255 0 1 255 252 0 7 255 240 0 31 255 192 0 127 255)
      #t
      ())
    #(13
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "zOE_vclqlG37jG2e12Lb0XMWMEVWifV6Q3_uEk3VTOw")
        ("y"
         .
         "rveAJsZTAwzy8xSmcGQjawo1Te_rxekMlBJOm_XE_CQ"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4)
      #t
      ())
    #(14
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "djPf0K0GdlCXvBG9UCKyAN8x8oxP8GJUISIax-625vQ")
        ("y"
         .
         "y5xnaTYJ3db5I0OlocY1QIJA9PjicSDBJVTH_4x24v4"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(128 0 0 63 255 255 240 0 0 7 255 255 254 0 0 0 255 255 255 192 0 0 31 255 255 248 0 0 4 0 0 0)
      #t
      ())
    #(15
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "o4as5XP4dVimjq0qIAiOP-kova6eEJRG-ToHjBV0HwQ")
        ("y"
         .
         "ISYebbK_EhBuTGv4W5WBtMAwKlJiIvkKvFpUkgaxEBE"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(255 0 0 0 1 255 255 255 252 0 0 0 7 255 255 255 240 0 0 0 31 255 255 255 192 0 0 0 127 255 255 255)
      #t
      ())
    #(16
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "jntQ99jETV00lsQxQaUC9KQ_FT0DrUPtqOOVl_HUd7g")
        ("y"
         .
         "ZH89pnlpt_mJ_0rdw5NRWvQMgghc4fLuGVQSxvWDd08"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(255 255 0 0 0 3 255 255 255 240 0 0 0 63 255 255 255 0 0 0 3 255 255 255 240 0 0 0 63 255 255 255)
      #t
      ())
    #(17
      "edge case for shared secret"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "yCf7kw_VHZJghhkbUCr4OrtfcX3ryN4piXo5NLJXHKA")
        ("y"
         .
         "WZDAWXsLei5C_r1WsTI10dQI127SyTs_rPUU2QL2kQo"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(255 255 255 255 0 0 0 0 0 0 0 255 255 255 255 255 255 255 0 0 0 0 0 0 0 255 255 255 255 255 255 255)
      #t
      ())
    #(18
      "y-coordinate of the public key is small"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "PLwbMbQ_F9wgDdcMKUTATGyxsIKCDCNKMAsFt3Y4RMc")
        ("y"
         .
         "T94KTvk4h0aXkycOsv8UgofakmWwM0-eJgmqwW6K1QM"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(127 255 255 255 255 255 255 255 255 255 255 255 238 207 34 48 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(19
      "y-coordinate of the public key is small"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KDDZZImuJLecrUJQVugnRvnj9BmrmqIcofuxHHMl59M")
        ("y"
         .
         "GKvmb1de6KLxxKgONSYK6CrX1vZh0V8GlnkwpYUJfvc"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 17 17 36 244 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(20
      "y-coordinate of the public key is small"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "RQtrbiCXF46dKFAQlRjSjrO23tKSKlRSADvC5KTsd1w")
        ("y"
         .
         "iU6Q8N8bDmytsDud4k9qItG9CkpYzWRcJzyuHGGb_WE"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 1 234 119 212 73 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(21
      "y-coordinate of the public key is large"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "PLwbMbQ_F9wgDdcMKUTATGyxsIKCDCNKMAsFt3Y4RMc")
        ("y"
         .
         "sCH1sAbHeLpobNjxTQDrfXglbZtPzLBh2fZVPpF1Kvw"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(127 255 255 255 255 255 255 255 255 255 255 255 238 207 34 48 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(22
      "y-coordinate of the public key is large"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KDDZZImuJLecrUJQVugnRvnj9BmrmqIcofuxHHMl59M")
        ("y"
         .
         "51QZj6ihF14OO1fxytn1F9UoKQqeLqD5aYbPWnr2gQg"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 17 17 36 244 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(23
      "y-coordinate of the public key is large"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "RQtrbiCXF46dKFAQlRjSjrO23tKSKlRSADvC5KTsd1w")
        ("y"
         .
         "drFvDiDk8ZRST8RiHbCV3S5C9banMpuj2MNR455kAp4"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 1 234 119 212 73 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(24
      "y-coordinate of the public key has many trailing 1's"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "mg8OPdMUF7vZ4pi8Boq21cNnM68m7Wdnb0EMgEuLLKE")
        ("y"
         .
         "sCyC86YaN223lWJulABVcRInOjbN2wjKqkOVOWVFRzA"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(127 255 255 255 255 255 255 255 255 255 255 255 202 8 144 17 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(25
      "y-coordinate of the public key has many trailing 1's"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "jl0i1eU-x5fFXs1ooIp8M2HNmcp_rRpo6oAqaky1ipE")
        ("y"
         .
         "jqegcCPvZ2dwJL04QeGHxkswowo3UOsu6HP75Y-hNXs"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 31 107 209 229 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(26
      "y-coordinate of the public key has many trailing 1's"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KTqjSbk0qyyDnPVLinN98jBO-bIPpJTjGtYrMV3WpTw")
        ("y"
         .
         "EYGCuF70Zuuajof5Zh99AXmEwV6oIEP1NtHuam2VtQk"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 2 9 159 85 213 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(27
      "y-coordinate of the public key has many trailing 0's"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "mg8OPdMUF7vZ4pi8Boq21cNnM68m7Wdnb0EMgEuLLKE")
        ("y"
         .
         "T9N9C1nlyJNIap2Ra_-qju3YxcoyJPc1Vbxqxpq6uM8"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(127 255 255 255 255 255 255 255 255 255 255 255 202 8 144 17 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(28
      "y-coordinate of the public key has many trailing 0's"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "jl0i1eU-x5fFXs1ooIp8M2HNmcp_rRpo6oAqaky1ipE")
        ("y"
         .
         "cVhfjtwQmJmP20LHvh54ObTPXPbIrxTRF4wEGnBeyoQ"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 31 107 209 229 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(29
      "y-coordinate of the public key has many trailing 0's"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KTqjSbk0qyyDnPVLinN98jBO-bIPpJTjGtYrMV3WpTw")
        ("y"
         .
         "7n59RqELmRVlcXgGmeCC_oZ7PqJX37wKyS4RlZJqSvY"))
      #(("crv" . "P-256")
        ("d"
         .
         "Cg1iKkfkj2vBA4rOQ4xvUoqgCtK9HaXxPuRr9fYz1xo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dGGMuq9p_1kPX7WFUc5KlItcclHUDllaGLG6a77mraU")
        ("y"
         .
         "v_QDqOmdU6cNPORhC_0F1Lo6iFW2oNNjyB99B4zezZI"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 2 9 159 85 213 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(30
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "ZkhceA4vg9ckM71dhKBrtlQcKvMdrocXKL-FahdPk_Q"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(207 228 7 124 135 48 177 201 56 69 129 211 107 255 85 66 188 65 124 158 255 92 42 252 185 140 200 130 155 44 232 72)
      #t
      ())
    #(31
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAP____________________8")
        ("y"
         .
         "TyuStMWWpaR_iwQdLepgQwIax3uagLE0OsnXePT49zM"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(73 174 80 254 9 106 108 210 102 152 183 131 86 178 200 173 241 246 163 73 15 20 227 100 98 159 122 6 57 68 37 9)
      #t
      ())
    #(32
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAD__________wAAAAAAAAABAAAAAAAAAAE")
        ("y"
         .
         "OBIL5qsx7fo0doxDh9L4T7SwvoqamFhkoVdfRDa7N7A"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(90 19 52 87 43 42 113 30 173 139 70 83 235 49 12 216 217 253 17 67 153 55 154 143 107 135 46 59 143 221 162 217)
      #t
      ())
    #(33
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAP____8AAAAA_____wAAAAD_____AAAAAQAAAAA")
        ("y"
         .
         "RiwEZuQYAiONbJJey-_HR8_lBeoZavmi0RtihQ_OlG4"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(199 55 85 19 59 107 155 75 42 0 99 28 188 121 64 236 190 110 192 143 32 68 128 113 66 46 51 98 242 85 104 136)
      #t
      ())
    #(34
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAD____AAAAP___8AAAA____wAAAD____AAAAP___8")
        ("y"
         .
         "FYL6MuLUqJ38-z0LFJ9mfbozKUkPTWTuKtWGwMnoxQg"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(6 250 16 89 147 94 71 169 253 102 126 19 244 105 97 78 178 87 204 154 126 63 197 153 191 185 39 128 213 155 20 109)
      #t
      ())
    #(35
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAD__wAA__8AAP__AAD__wAA__8AAP__AAD__wABAAE")
        ("y"
         .
         "aEyKlYbtb5y-RHBYp9ohCLqx5eCmDR9z5OLnE_Cj3-A"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(242 55 223 76 16 189 62 53 121 113 187 43 22 178 147 86 107 126 53 91 220 129 65 214 201 44 171 198 130 152 60 69)
      #t
      ())
    #(36
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "CF7FpK9AF2tjGJBprv_LIpyW0-BG4Cg-0vnawhsVrTw")
        ("y"
         .
         "eFn5fLbiA_Rr80OPYSgjJelOaBtgtWaXiK6wZVvxnTg"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(216 116 181 86 120 208 160 77 33 108 49 176 47 58 209 243 12 146 202 175 22 143 52 227 167 67 53 109 146 118 233 147)
      #t
      ())
    #(37
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "GQwl-IrZrjoJjmz_5v0LG-pCEU6wzt1YaKRcX-J33_M")
        ("y"
         .
         "Ibg0LvB3vGckESQD6u5aFbTDGnFYnwLe0JzZnMXbnIM"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(17 168 88 32 87 70 63 199 111 218 58 184 8 126 176 164 32 176 214 1 187 49 52 22 90 54 150 70 147 30 82 166)
      #t
      ())
    #(38
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "UHRCAHMiqolTQMukq8LXML_QsWwseaRoFfh4DSxVot0")
        ("y"
         .
         "RhnWn5lA9RZjqhI4G8fPZ4vRpypJ-8EbC2nLItGvny0"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(78 23 58 128 144 127 54 31 229 165 211 53 186 118 133 213 235 169 62 157 252 141 143 205 177 220 210 210 189 226 117 7)
      #t
      ())
    #(39
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Xxd7_hm6qu5Zfmi2qHpRnoBenSinDLcv1A8P5adUukU")
        ("y"
         .
         "YsoRA_cKIAbNH2f19qNYCyncRGq8kODpEMHgWpqniM0"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(115 34 4 113 236 139 173 153 162 151 219 72 138 52 162 89 249 188 137 31 250 240 153 34 230 181 0 31 93 246 112 24)
      #t
      ())
    #(40
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "f_8AAf_8AAf_8AAf_8AAf_8AAf_8AAf_8AAf_8AAf_8")
        ("y"
         .
         "LiITyvAwM-D9D3lRFU9ubDqSRKcvrKZenOnutcjhzqk"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(85 208 162 3 226 47 251 82 60 141 39 5 6 12 238 157 40 48 139 81 241 132 190 239 197 24 207 246 144 186 211 70)
      #t
      ())
    #(41
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ")
        ("y"
         .
         "K-h4nbgbtIcKnmDFwYyAyD3kZCdygfGvHmQIQ6GjFI4"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(37 24 216 70 229 119 217 94 158 123 199 102 205 231 153 124 184 135 251 38 109 58 108 181 152 168 57 253 84 170 47 79)
      #t
      ())
    #(42
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gAAAP___8AAAB____gAAAP___8AAAB____gAAAQAAAA")
        ("y"
         .
         "ciVA-KRxw3kIPGALWP3k2Vx9ytUJX0IZ_F6b3ePFzTk"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(189 180 159 75 223 66 172 100 80 78 156 230 119 179 236 92 10 3 130 140 91 62 250 215 38 0 86 146 211 92 15 38)
      #t
      ())
    #(43
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_wAAAAH____8AAAAB_____AAAAAf____wAAAAH____8")
        ("y"
         .
         "XfgPxsria2wZUvvQDtF07hIJ0GkzX1tIWI4p6AuRka0"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(245 3 172 101 99 126 15 23 203 68 8 150 28 184 130 200 117 228 198 239 122 84 141 45 82 216 194 246 129 131 140 85)
      #t
      ())
    #(44
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__8AAAAD____8AAAAD____8AAAAD____8AAAAD____8")
        ("y"
         .
         "LGNlDmpdMy4ph90Jp5AI6PqrvTfknLAWv7ksjND12nc"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(227 193 142 125 115 119 220 84 11 196 92 8 211 137 189 190 37 95 168 12 168 250 241 239 107 148 213 32 73 152 125 33)
      #t
      ())
    #(45
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____wAAAAAAAAD_________AAAAAAAAAP________8")
        ("y"
         .
         "ehFslkpM1gZov4nP_hV3FKPOIbk7PKYHyKW5OsVP_Ao"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(81 109 109 50 155 9 90 124 126 147 180 2 61 77 5 2 12 20 69 239 29 220 179 52 123 58 39 215 215 245 114 101)
      #t
      ())
    #(46
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "f_______________7s8iMP____________________8")
        ("y"
         .
         "AAAAAcfDBkOr7QrwpJ_jUstIP_m5fczfQnxljoeTJA0"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(111 210 102 97 133 26 141 227 198 208 111 131 78 243 172 184 242 165 249 193 54 169 133 255 225 13 94 235 81 237 207 163)
      #t
      ())
    #(47
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "f_______________7s8iMP____________________8")
        ("y"
         .
         "_____Tg8-b1UEvUPW2AcrTS3wAdGgjMgvYOacXhs2_I"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(111 210 102 97 133 26 141 227 198 208 111 131 78 243 172 184 242 165 249 193 54 169 133 255 225 13 94 235 81 237 207 163)
      #t
      ())
    #(48
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "f_______________ygiQEf____________________8")
        ("y"
         .
         "Jnv9-KYRSN7NgCg3Mt1MEJXku0C5ZYQIII3BFH____8"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(68 35 108 139 149 5 161 157 72 119 74 57 3 192 41 39 89 176 248 38 230 172 9 47 248 152 216 126 83 211 83 252)
      #t
      ())
    #(49
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "f_______________ygiQEf____________________8")
        ("y"
         .
         "2YQCBlnutyIyf9fIzSKz72obRMBGmnv333I-64AAAAA"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(68 35 108 139 149 5 161 157 72 119 74 57 3 192 41 39 89 176 248 38 230 172 9 47 248 152 216 126 83 211 83 252)
      #t
      ())
    #(50
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAEREk9AAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAADRLTgbB2CxxQvorPhZOFBSx_U83mfOE3Wd4xI6A"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(241 240 228 59 55 79 235 126 127 150 212 255 231 81 159 168 187 108 60 253 37 246 248 125 171 38 35 210 162 211 56 81)
      #t
      ())
    #(51
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAEREk9AAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "____8e0sflCJ9OOvQXUwemx6-tSArDIZgx7IpiHO3F8"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(241 240 228 59 55 79 235 126 127 150 212 255 231 81 159 168 187 108 60 253 37 246 248 125 171 38 35 210 162 211 56 81)
      #t
      ())
    #(52
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAH2vR5QAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "QJbt1occMgy4qfRTF1EQXJe0wleBG7wylj6vOf____8"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(62 187 172 225 9 138 129 148 157 86 5 221 148 167 170 136 220 57 108 44 35 224 26 156 140 202 91 176 123 251 182 161)
      #t
      ())
    #(53
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAH2vR5QAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "v2kSKHjjzfRHVgus6K7vo2hLPal-5EPNacFQxgAAAAA"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(62 187 172 225 9 138 129 148 157 86 5 221 148 167 170 136 220 57 108 44 35 224 26 156 140 202 91 176 123 251 182 161)
      #t
      ())
    #(54
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAB6nfUSf____________________8")
        ("y"
         .
         "AAAAAHr7wLMl6CBkbexiL7VYpRw0KqJX9Lao7F3fFE8"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(27 8 82 19 169 200 157 53 62 17 17 175 7 140 56 197 2 183 180 119 30 251 165 31 88 155 91 226 67 65 123 220)
      #t
      ())
    #(55
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAB6nfUSf____________________8")
        ("y"
         .
         "_____oUEP03aF9-bkhOd0EqnWuTL1V2oC0lXE6Ig67A"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(27 8 82 19 169 200 157 53 62 17 17 175 7 140 56 197 2 183 180 119 30 251 165 31 88 155 91 226 67 65 123 220)
      #t
      ())
    #(56
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAACCZ9V1f____________________8")
        ("y"
         .
         "FSwaItgjonhV7QP44qtQOLsd9Nh-Q4ZfLa9pSP____8"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(103 203 99 86 108 124 235 18 253 216 92 233 210 247 124 53 146 66 187 170 14 161 191 60 245 16 164 162 101 145 209 241)
      #t
      ())
    #(57
      "edge cases for ephemeral key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAACCZ9V1f____________________8")
        ("y"
         .
         "6tPl3CfcXYiqEvwHHVSvx0TiCyiBvHmg0lCWtwAAAAA"))
      #(("crv" . "P-256")
        ("d"
         .
         "VdVfEbuNoeoxi8pyZvA3ZmJEHqhycKogd_G3cMSFSkg")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SxZj7h2TMLcrIL6yg5hQme_3tAel6XekWoJd4p3uluk")
        ("y"
         .
         "6dB0rkqADlx2fpnriM0dV7BXimIO0r61V6hPP3YgGnU"))
      #vu8(103 203 99 86 108 124 235 18 253 216 92 233 210 247 124 53 146 66 187 170 14 161 191 60 245 16 164 162 101 145 209 241)
      #t
      ())
    #(58
      "point with coordinate x = 0"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "ZkhceA4vg9ckM71dhKBrtlQcKvMdrocXKL-FahdPk_Q"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(209 28 100 11 67 130 230 14 200 210 84 238 118 240 155 143 172 87 101 26 183 59 109 211 253 201 53 166 21 100 163 233)
      #t
      ())
    #(59
      "point with coordinate x = 0"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "EAEh8aCUQ4UcmqKrbuZEDirF4b5kgnS9XSbBL7O6P38")
        ("y"
         .
         "AyocIZ-hRXyyBYgpfgUTz9SQH5qVQU9-kU-RefOFZ6Y"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(144 231 18 226 175 209 65 113 193 148 103 162 191 231 171 241 196 119 209 244 15 102 117 240 14 98 47 213 96 79 161 106)
      #t
      ())
    #(60
      "point with coordinate x = 0"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ytAqtTfICDHM3TlRKfxL_kqJrgyGb2YZo-FBRtNpFpQ")
        ("y"
         .
         "aJ1HcGW0DxQO2Hs3rQQeKCKbD3mms8mSaJlUyX9zNtA"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(21 149 131 16 61 131 246 53 56 189 78 32 54 7 215 52 137 144 187 127 132 127 251 201 229 229 9 199 227 77 57 44)
      #t
      ())
    #(61
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "q9Eu7U1lS6p9loYzdw9KWC8XPWYzkGAA7YrPYjPGNl8")
        ("y"
         .
         "CRLzC7mOfLUliQ1eoeIXFJ1SpsWfeAKp8wfoDSqf7jo"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(84 106 45 250 219 29 96 20 11 236 172 45 194 230 45 32 199 137 3 119 85 173 90 73 227 126 72 242 202 27 118 128)
      #t
      ())
    #(62
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "pWLBrZpyIX3wAUfH0s6vxloWIKFGnJR-FP5DADrFNxs")
        ("y"
         .
         "etHTPAHw65K3ee1uRg0DNERwdaPPZrL_va4xtDjfbXs"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(229 133 156 120 17 197 195 172 166 194 54 171 73 156 202 209 3 1 199 197 238 145 60 233 27 182 100 40 205 225 30 77)
      #t
      ())
    #(63
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "jNvr6dB9LrxOQbHXKpusKXTPxM9zjYtt5xpA7emSDYg")
        ("y"
         .
         "3CQ57gAD-957CjrkFxDGSxewiohB6Xo5Dkgsl2j-Aeo"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(101 117 74 180 89 161 4 113 175 0 148 63 65 79 40 222 27 195 121 104 176 151 173 40 69 254 17 20 32 133 80 8)
      #t
      ())
    #(64
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "8M182DNGeDCM_reFpooVBKkUGNREHE1MdAxXSIuar7A")
        ("y"
         .
         "edio0plz61AiZ-zPbtoyZib8bgJdUyuF6fcR-M5pcbs"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(134 49 254 222 230 206 179 56 106 196 46 223 50 44 24 136 36 137 61 38 125 97 8 240 207 93 230 150 75 136 51 27)
      #t
      ())
    #(65
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "itCvI7kOA0G04qWpY8hSL-ARrOGbG4YQy-eSehenJJc")
        ("y"
         .
         "Nrh6uZByiaI6D7IMpL5C1CH-ONNa8J15y-bmpOlaGos"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(104 197 133 153 193 35 190 109 55 211 67 189 65 177 28 236 197 248 75 38 53 102 17 99 101 111 118 215 251 4 180 38)
      #t
      ())
    #(66
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WcnMLXKX3bC-YwTJTOv0LYE-lwxQ9FKHdTuOnLDG20U")
        ("y"
         .
         "9XHZhpkIl4UfyOHbZ8mXWeiXnD2d39AvYzzx6ltsSKs"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(181 141 0 82 92 76 75 79 70 86 40 82 193 92 226 228 141 190 35 163 190 55 84 30 4 132 70 239 245 21 46 198)
      #t
      ())
    #(67
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6XCA2nJjopwwcqZReLezFYel3_wZdUxWHjL8UxmSNPA")
        ("y"
         .
         "TgubcMl7YOlA1WKfImbRqOJC3rcet_CystouMERzirA"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(75 170 1 194 17 175 143 148 172 168 149 72 144 42 113 247 181 63 120 20 187 206 179 212 190 243 27 55 110 52 180 118)
      #t
      ())
    #(68
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "RPYA2nFguXWgIyy2pKnnKAP9d8qshDUgOc6fSmeh2nc")
        ("y"
         .
         "YmBFWZOB5ZnrnNA_KC4me4z9O6mNq7sPKascCUQnDz8"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(225 159 233 209 41 76 202 148 166 56 136 37 36 158 107 55 147 26 35 30 185 23 207 236 178 146 121 45 12 24 241 184)
      #t
      ())
    #(69
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "cePpvg4O5ESaGdLveRkmaBSg-v0E-2d-3DJlbmpG5NI")
        ("y"
         .
         "vF9ATFtU8D4pS-IuiCCnG01KwEpwjhPNcf2wBB5-lpg"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(221 193 244 102 59 146 138 221 6 177 229 124 72 219 152 234 8 196 211 60 60 33 6 55 20 7 243 132 138 157 83 247)
      #t
      ())
    #(70
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "J7aTYQFU1bfwgJTkb_KirBwB082CbjII5SVENu0nmWA")
        ("y"
         .
         "8jZOOmBPO1kuGSYqGyKxoUjjjNgsnlTxCO-PgzaD-LQ"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(145 223 169 94 209 234 203 234 65 145 86 71 26 141 219 182 203 147 221 69 100 51 225 134 51 210 104 23 97 27 156 100)
      #t
      ())
    #(71
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "wypSr22sNptqSZpJ0-OOfJU0u5E59X1JhLHTwEq4IgY")
        ("y"
         .
         "U83C2u-sg89DwNZGBOX52FtV3eYraSzTavmev_QUDDk"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(159 145 169 99 61 170 76 86 70 94 159 190 244 67 30 19 4 31 104 145 15 181 186 137 248 218 147 129 214 138 13 254)
      #t
      ())
    #(72
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "b04vcvMq5m8fRhCWYATENqoNkLffB86cSspSsC1GtNA")
        ("y"
         .
         "xqPsdr8yG3_lIDzz1m4tUuPuBJXsdm1XmkURF14BvE0"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(1 74 232 20 66 248 203 109 245 143 244 30 109 178 3 219 64 234 149 27 145 190 191 134 212 44 218 123 227 63 234 100)
      #t
      ())
    #(73
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "LgZZdd9kL8_a_i-lr_wYssaDcXlvnZY9icT1rFzOoos")
        ("y"
         .
         "mQ8xUi-7Jlw_TVxLuC6_Xd_1qOpYjbTSgqzcp6bM9Cg"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(120 232 30 133 115 195 174 96 137 223 125 177 251 41 215 190 18 220 17 241 91 178 91 255 42 248 2 225 93 220 19 110)
      #t
      ())
    #(74
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "4TMe7gPFDMK5CUTd_A06fdgYXmwhx1-pKgwUsPGUmsk")
        ("y"
         .
         "FU14P0VH3PVQi72Gw92MOxe2GYn5PbVJDsAqRqEAXCw"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(237 103 25 90 39 44 99 197 2 5 171 242 116 57 41 17 52 255 161 232 236 89 127 59 48 39 22 217 54 50 233 141)
      #t
      ())
    #(75
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "4MVtSG6cARY-1sP_Jd4831dE2_ng4Avc8Zll30uh8xE")
        ("y"
         .
         "vV5EQwZlgj2MCzTr7Apqq16pbPI53iFP0BHm-exQHdQ"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(80 119 67 71 132 136 40 238 182 35 15 73 124 209 129 248 197 127 189 24 255 191 131 40 205 0 131 33 161 195 124 67)
      #t
      ())
    #(76
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "iF6tbAdPjXUadn6RjE6JIQpYfEsZ1CJErgcCfjYYMQU")
        ("y"
         .
         "PoB3K-V_vXRJVaLoUjBjzGE28rs3vvvvemgdO7vFd4g"))
      #(("crv" . "P-256")
        ("d"
         .
         "5GHFteY9dbTIwSO_i5zUXnEq8I9-LklKjyVaydgOBYs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sMgyoyToCtAo9y_5vY9fDRqtXBIdxMvrd95A2KcPSoU")
        ("y"
         .
         "knhyGyOEmb-wyXhdby3KK8n2NlGkWLVVEKInVL494gM"))
      #vu8(145 61 167 16 68 184 2 26 134 200 252 175 79 99 77 13 98 95 249 30 225 200 71 77 84 139 209 8 136 150 79 177)
      #t
      ())
    #(77
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "QenUz6jv6AuJWoy8ziVo4lHbfs39IKetcQ1KS_Kt3Gs")
        ("y"
         .
         "XsNqgzkWigPxW4yA8qKoKPFR04eRWEhTui_0SioEYKE"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(180 142 17 157 41 238 247 219 183 107 100 33 142 114 141 219 246 236 96 5 5 236 124 237 106 182 251 135 99 48 141 165)
      #t
      ())
    #(78
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "d2rvGsuCtijhMswpRAmI8KFdTMK08yiuywY8m4blAY4")
        ("y"
         .
         "bkTfxgRE-qnE42vCF0UfesKVbLOy6bvWVeuilxY9HzQ"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(40 168 139 107 37 143 35 48 32 186 111 169 192 13 29 114 131 31 69 21 184 105 102 169 120 47 82 19 21 225 138 167)
      #t
      ())
    #(79
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "nsBrCwhmLA4d2REWlqY6FgHMg87iBpV3it-E1DBk_JA")
        ("y"
         .
         "FWAB8ITNPB3xoIf2JlM7ZXJYSIm9PVwsmfDjEeIrQeY"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(196 255 134 95 243 220 73 83 234 120 217 42 2 243 52 90 83 189 182 5 12 253 143 65 186 164 57 94 203 106 202 184)
      #t
      ())
    #(80
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "-lHRKK3CAA8J_xLG_Y4lqghVbXCL9rD__56OqtR4Pw0")
        ("y"
         .
         "4iv1KeUW4fZLjg0J-Y-tTlAWlakwobIgdmWdpwfjzNA"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(222 16 105 240 81 99 126 16 22 101 89 206 244 70 136 175 200 9 52 24 85 38 18 21 196 243 129 217 215 218 118 202)
      #t
      ())
    #(81
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "YU3PvqR4mj8-tKji8RHIh_AkjZMWuZ0IZMknoEXWlBc")
        ("y"
         .
         "U6Bzvv4ISRqAUKTZbQi6R5CuGNs-9_DqzPWc4Qla_FQ"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(66 7 191 65 89 250 160 229 14 210 56 185 192 255 70 25 74 83 154 27 160 58 90 76 141 104 243 105 174 205 49 165)
      #t
      ())
    #(82
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "7-d1TtTAs8HdMBvB7WmACqL_XVH7hZN3FeYNLnvK2o4")
        ("y"
         .
         "sVgat1-zx5fvlKnbo9glaMhGF-rz-gTyefv9iY9wRgQ"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(181 160 236 146 174 204 48 16 210 125 34 99 211 218 102 227 210 243 57 93 35 148 112 36 163 244 116 68 84 98 32 39)
      #t
      ())
    #(83
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "2OE_vQF_H5omvjXGEdeyKZ9dEN48iiY2InP_-4Ujjz4")
        ("y"
         .
         "0UJrdIwfh-OvosHnoCJDEMmAZV4HOZWQ0UlNbWvqA5Y"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(210 165 188 102 73 140 96 54 174 205 250 173 4 28 239 115 42 137 61 225 144 160 165 180 47 247 30 19 240 146 128 231)
      #t
      ())
    #(84
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WhAnZmoONySB_sCzkB4FjWAQfAexEVVQzrBXibVabTU")
        ("y"
         .
         "Bj1MjuZu1F_z4d_c_XPtlqnoMZOIStvKpXSy3RGKaSs"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(31 129 35 19 221 207 54 188 56 7 29 14 81 167 65 0 214 48 200 226 12 196 20 50 110 239 164 46 203 27 95 142)
      #t
      ())
    #(85
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "eTe5xAmG3XVaBlYgMIl4JYPafYETpEGQdiq0dKILz2A")
        ("y"
         .
         "78vBUlrtW0rY5ofLAsLviIcJXK3KVsdltBtKlUT_L-g"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(242 132 8 155 221 213 226 225 190 63 130 100 14 250 6 88 70 143 161 241 11 40 25 99 163 202 25 12 57 130 253 166)
      #t
      ())
    #(86
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "k2gGagdIhnp7hwJE9cn4Lqi9UVUpWd1VC7c5RJcVml0")
        ("y"
         .
         "QHZK3RriTI4_Qy7gEb6X0xMHGP4KapDtixARsgNNCaA"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(69 41 244 182 49 201 152 74 178 22 166 128 18 129 252 79 216 115 26 88 182 92 168 208 123 255 7 129 17 22 55 31)
      #t
      ())
    #(87
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "mB10Sb3wAT9e7du35CxEL3zN2UJ70m17OIdVql4m9Go")
        ("y"
         .
         "EpK4j6a_Xf_KBU3ULtNZQne1k9zEAtgDQPt4FuTcqzc"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(100 187 201 253 215 54 67 235 41 84 244 171 100 3 129 185 56 197 230 1 132 106 12 107 105 84 150 110 13 199 62 111)
      #t
      ())
    #(88
      "point with coordinate y = 1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "CeeNTvYNBfdQ9mNiCQkrxDy91rR-EaneIKn-sqULuWw")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(40 246 119 87 172 194 139 22 132 186 118 255 213 52 174 212 45 69 184 179 241 11 130 165 105 148 22 239 247 25 154 116)
      #t
      ())
    #(89
      "point with coordinate y = 1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "U4TWwN73iWDblnuAltNUd8WlzjDvDG2IeaVWjKh-l5Q")
        ("y"
         .
         "Ae5WxFgXImELQ_PL_POGLAgqbja6o2_W94QDwOOZ-qU"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(158 230 83 205 164 109 182 118 18 118 12 227 91 172 132 80 187 244 141 191 116 69 30 217 58 187 109 180 8 169 254 16)
      #t
      ())
    #(90
      "point with coordinate y = 1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Tsp2QaSv1eqwshRlf_O9y_xm8VUaU7tZSTvDjteP85Y")
        ("y"
         .
         "FKDK3_FMFHNu29zatRDLoHqJJP_QSQ7lFK7fqttkiwE"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(151 54 173 107 42 46 241 126 195 248 200 220 46 53 113 95 177 192 111 40 216 46 78 38 135 111 2 20 88 129 101 241)
      #t
      ())
    #(91
      "point with coordinate y = 1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "jQF366ucbp4Q223QldusDWN16Kl7cPYRh12HfwBp0sc")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(116 143 164 245 163 153 50 3 130 220 146 0 38 147 134 148 196 26 38 254 42 170 49 140 94 113 1 152 221 113 199 147)
      #t
      ())
    #(92
      "point with coordinate y = 1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X9t_DP-4tbEULSRpikvadr-YJ9Y7GmvYWk4vm1nFEM8")
        ("y"
         .
         "vLNbqcmHEIttQzetU5P5-RDskkEMIwhp1mUo7YjBuYo"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(127 151 219 131 180 216 111 4 254 40 96 65 238 33 232 14 195 213 159 60 232 44 222 234 243 98 1 111 200 122 62 2)
      #t
      ())
    #(93
      "point with coordinate y = 1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Uwsik-YMa28Ux1yQse-Ln5-mshUbjZhVeS6ys9xp8Ho")
        ("y"
         .
         "DbQkQOc_19bfBK7VAi--Ic6uwzxfut4b1q0yHvLhDQs"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(33 121 76 242 79 86 39 63 164 70 60 199 174 66 50 250 52 219 224 241 139 115 97 59 138 233 203 251 156 54 171 240)
      #t
      ())
    #(94
      "point with coordinate y = 1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "aRb6xF5Wi2ueLi7NYRsoLl_MQKMGfWAQV_h5zlqKc8w")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(145 81 6 208 120 22 232 121 231 100 63 0 171 246 215 159 184 241 203 120 191 100 166 163 130 127 145 167 176 239 15 65)
      #t
      ())
    #(95
      "point with coordinate y = 1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "7ZVoyFvFKmtFczYYw2AhB8H9rPI7Gjjkhq-Vl4ohTi4")
        ("y"
         .
         "-g1x1ec3iRxCduJHWB7mE5ARyhRg25seILNk2SdWg-I"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(47 204 229 82 49 8 25 221 119 90 183 186 159 240 249 106 31 202 221 37 160 199 9 112 60 239 4 187 110 26 123 215)
      #t
      ())
    #(96
      "point with coordinate y = 1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "n_dzHADyqoiz_BdKupB60XWV5gLnaKXx6UYqbUuJstI")
        ("y"
         .
         "PxeKcLm7PtziiRGDOKM98wxDLDR_EqPeCisDs1OHjZY"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(117 125 146 106 38 147 188 138 61 45 140 5 84 161 53 121 239 158 85 145 134 87 137 17 243 126 220 136 178 245 230 26)
      #t
      ())
    #(97
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gnD4F51XQ2s038C999QXpciVEWuQy1GuxxhhT4ZKY10")
        ("y"
         .
         "F0gE4MDgbj1o0xSeC5VmIcaqK96D9NF9A9KO-Ko4n_8"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(61 178 158 198 249 120 210 38 158 146 233 199 235 92 139 90 142 86 194 34 138 79 185 228 131 254 202 80 170 62 69 31)
      #t
      ())
    #(98
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "xhdQ6Yq68gIlqIHb_TUQUyz8Pflxu7ykor1S-RrMnFk")
        ("y"
         .
         "0P55NCCX-Irnj8eagDIkX90sMMxkrOqqn9V7CCVpJTE"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(114 197 124 46 16 215 115 24 179 167 150 9 123 191 118 140 99 102 20 45 128 249 140 144 169 55 128 168 65 7 95 50)
      #t
      ())
    #(99
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "nF07tUZQ2VUOHuLvo-pDwUq5nRi7BJ83tCptrEgjLws")
        ("y"
         .
         "06J2DYPTOv5M5vHRJFSJxQm9JrAlHzCPjJlugPej-Os"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(169 107 7 148 78 158 178 178 42 154 54 87 94 255 31 79 99 99 180 170 58 83 177 0 184 81 138 103 186 84 5 221)
      #t
      ())
    #(100
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "8XJO_Uga1FpVeV8GEmsfXtKOfZu0_ukQryrYwTc7GP8")
        ("y"
         .
         "d-28NNpseH7HNDA0f02oaBADLYj3R19sQvFZFAedF54"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(133 88 131 49 107 109 9 122 229 234 182 198 126 132 17 161 57 115 73 160 155 157 125 143 9 107 43 161 189 3 234 49)
      #t
      ())
    #(101
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_DaAr1L6if_NGT7MCwcURm_l2yd-5YcoRsUgv043Idk")
        ("y"
         .
         "JyYKDiJaPTd-ZyPstr741Ek8LaeKIqMH_MqPiPRScgg"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(90 117 187 122 12 150 184 52 13 8 66 188 204 241 25 116 225 165 162 200 244 188 34 179 51 67 60 206 100 107 106 138)
      #t
      ())
    #(102
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "EGtvgeNILbGNdAKSkYIa5EjDiETveDvx1pmaQEQB9j8")
        ("y"
         .
         "aldT8O3GimLP1qCxgbslmeHzusX6iCSvFg3nnthnw1A"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(217 100 18 227 28 244 210 97 149 146 12 172 149 47 183 158 162 95 108 80 171 199 155 94 208 239 128 38 166 232 51 25)
      #t
      ())
    #(103
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "CTy1GTpPlM0Y7aogqXO4f_ebDANoTHlIfs_uNH5TVOs")
        ("y"
         .
         "BPy1dSU5Fwd3kyvhXNhMl_A4Ff_ui2C2R8F47ruOFNQ"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(43 14 237 155 173 201 42 16 104 25 109 254 193 36 254 143 157 63 69 30 41 77 50 46 184 129 204 224 47 40 96 38)
      #t
      ())
    #(104
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "1sOPRIuWTie1tFDMONPPQe-d-D2KlZdx65whhVyzZEU")
        ("y"
         .
         "32OK70airrExmSgeGibRL-YbAp7H9ouQ-qifiMepWUI"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(237 11 29 141 253 39 166 31 206 145 220 100 5 191 197 59 109 72 168 193 59 165 65 201 110 243 220 243 29 124 219 136)
      #t
      ())
    #(105
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "inSNYfWcO2optzOw1VSySS5_dvrXyuHBfyrD3p5KZdI")
        ("y"
         .
         "7tvmwmtv0iv8A8FodVXS8KOOAq3uVXBoYXGr_sZoGRc"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(167 150 221 20 79 33 186 51 24 249 225 8 40 236 239 201 192 246 239 44 66 122 227 19 81 193 108 47 191 163 207 166)
      #t
      ())
    #(106
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "8QUmmdh-VnfHXiayq-cZMQZI2CCpbls4H_9Ys5JAFYE")
        ("y"
         .
         "sbsWrotoy7dqMlaHC60e5aMP-f1mL9T40f5bXx-Y_0Y"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(31 58 150 21 176 116 80 70 169 114 186 213 213 151 148 160 182 11 3 43 74 201 79 232 95 119 223 179 128 209 243 43)
      #t
      ())
    #(107
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "EhmvUjAGTul3hmciXw4AnNuWEzDjhu2zTk-p_d0OW-c")
        ("y"
         .
         "4qElVCJ_YTqqp4k43bvJm5I_nRgbgZLcS4Fld-jzt-k"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(202 249 20 29 31 202 77 15 16 104 59 94 134 210 180 26 245 96 47 1 121 145 254 115 72 212 78 141 112 20 17 92)
      #t
      ())
    #(108
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "YN7xMPGQ5txE9euKWeEufvsn25aMf6bMbTF4XwZrQbE")
        ("y"
         .
         "8btVasTNdwM-eqbFuhb0frr7FJdaf9ct2bf-IxFrylU"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(101 57 236 28 152 250 117 25 123 160 124 103 139 38 48 11 61 161 254 64 125 212 198 139 137 69 126 214 105 8 46 6)
      #t
      ())
    #(109
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "8j8JvbfRconrAFl1p1ejkyW035sp5VuiymebXsCXOuk")
        ("y"
         .
         "GMiB88e2wSvtHsVLg30IxZCOib3O3YS5F3cgN494lgA"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(11 102 25 130 124 250 148 141 99 240 33 233 237 219 146 248 132 251 92 232 164 4 191 224 89 233 147 252 35 68 122 105)
      #t
      ())
    #(110
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Xb7AmMG33j4-LnPQtizUnId-GgEwobOesv1NvUQmqkw")
        ("y"
         .
         "y-7iF1kajXbMjerxTd5S4_QB5Tswy7nBgHkQ2CfQBB0"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(42 83 165 97 172 245 202 236 110 176 216 170 64 114 121 66 136 26 117 209 54 137 157 251 255 145 82 130 54 146 108 57)
      #t
      ())
    #(111
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "HnBzDcTznIlwGC4aKcyDa56dbL1vyqjA3BBi_tmoSWk")
        ("y"
         .
         "PnuRUfnIozRTZvgiHI-3AOjDqap_DMRqSIZOFgVZIJQ"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(155 55 119 22 255 29 5 109 172 142 57 34 73 234 236 116 13 47 90 166 35 3 244 186 246 187 27 3 178 162 118 197)
      #t
      ())
    #(112
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "9CjJrj4j6vnCpbmn5B79HP-_NfiBv8NWlNnAXR4xKxA")
        ("y"
         .
         "722pAjz9LdDLe54qd9ZEr_5ipj-w8p1FKRxoYaoGPFw"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(12 12 104 103 102 151 67 8 37 71 170 148 69 31 235 54 47 162 159 186 242 40 223 179 234 243 117 241 165 236 47 179)
      #t
      ())
    #(113
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "uaFtmluFpxTiuyqiKwhqF0BMej_2JFJzI0dBnJnpC9o")
        ("y"
         .
         "1Xi0YvUjmUMEtq_PaUSpzF0K0a-tlWR1yPKVPAawa5c"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(209 31 158 50 88 127 211 182 244 162 53 72 18 97 139 75 59 74 117 57 184 162 35 179 136 187 116 55 248 209 56 165)
      #t
      ())
    #(114
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "j2WaFjpY6fkAwemzT7HNYf_JiQJnvjQXyK_nnVchTaA")
        ("y"
         .
         "XNXLaKK5PaDb5Wwc_A3Oi2wyYODEg3nG0gkfFrOSIcA"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(75 171 246 54 142 3 89 183 134 20 6 2 65 236 228 111 172 202 63 82 245 187 196 122 192 180 106 7 91 93 211 160)
      #t
      ())
    #(115
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "0lfxM_AKB59OZ3jqSpv0K58jEpBDG1uT1-iw41tIAQY")
        ("y"
         .
         "UNbGtGV00e_OA1ELjbSgmBzhOMW9j-DlTJiMQMX8kgA"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(150 39 204 92 141 139 114 39 139 232 156 50 181 34 16 23 62 111 75 142 47 72 228 96 198 66 159 70 249 244 105 174)
      #t
      ())
    #(116
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XvKsV8TpPPeNj4bDXUE7mNwZAt0kWv_eXBYDSvx-pFU")
        ("y"
         .
         "R7Pp93-8UHW60DxBgJTxrsHQPt6voWf6avg1JlUvcDQ"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(210 177 120 188 155 177 107 90 145 161 0 187 114 225 90 150 57 224 80 192 52 52 96 97 65 62 194 12 79 204 155 188)
      #t
      ())
    #(117
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "p7UT-WJmQU-m_0OaNdjwmrYV2wu2o7GhIMIXaD9ySyM")
        ("y"
         .
         "QgB6LJ_qvNYkmg0XrOzZleKiF_tfB77JaTgBbil--lI"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(108 220 160 167 49 175 241 204 251 25 4 167 105 206 247 158 186 150 95 186 177 204 100 210 4 157 13 244 93 204 210 118)
      #t
      ())
    #(118
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "d0Orckja5fGlmsawoTbp8eUa_4vUV5Ws5fgYehPt-a0")
        ("y"
         .
         "vZZCB4N4urXG1IT54c45Z1tyFwvzmryb55QvwB_ENdc"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(189 21 233 122 127 73 170 51 229 123 84 20 10 117 255 252 231 27 120 140 224 250 163 52 207 139 69 98 61 204 129 138)
      #t
      ())
    #(119
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "DjqpcbrNrONQ3AlX-lveCUYyTrE5k51_wZl8cB7_0Eo")
        ("y"
         .
         "Tmw2JdlWQWjTp1KWEiGh3oz189YDdSqMLmJ3rDqRjCU"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(200 181 232 231 72 136 87 162 221 230 44 95 194 30 69 37 235 171 160 224 107 91 232 62 198 231 221 119 30 21 160 26)
      #t
      ())
    #(120
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "D1Y-Ib-bJAFafNu28ACmknhKwuS8JxXHb2hCZKiZyCQ")
        ("y"
         .
         "DKsNduawHKvk8ydCnRG-EV7W3AynTwLBuYeggvWvQ6g"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(28 99 164 87 80 155 20 130 114 104 126 110 68 43 222 81 152 45 65 176 8 13 140 12 94 183 20 37 122 249 113 231)
      #t
      ())
    #(121
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XaSfECSeTfPbtOMezgsO6aoHPyWIGVqq5j509lZ6d0g")
        ("y"
         .
         "ELXdYba_IZ6eqzDvCcE_wYSz0J_3pOGSvKj1ERxBY8c"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(115 161 172 158 206 53 74 147 13 253 156 119 87 123 79 80 172 192 167 137 100 234 13 119 117 99 29 100 199 9 196 162)
      #t
      ())
    #(122
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "b3Lm5cYwBnnT8U8PblkGZWQ1dq6LvLfAWy9Kg-deasM")
        ("y"
         .
         "5xLLBW_wNNo0BUPF2mmX5lo6tM056ZeJK7ku4sIrgWc"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(252 170 64 99 41 187 116 249 149 134 44 234 124 236 199 66 92 107 212 20 142 241 169 244 107 93 66 218 89 148 85 106)
      #t
      ())
    #(123
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "a1RN-RaOd4fbKC4q4B3XIwbZybyA9as4zllHZsPZKek")
        ("y"
         .
         "Z0k_9gHKYIYrR9OgeFyRfkRYQETjYCOlRCQBXli-UEA"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(228 159 241 29 70 182 196 181 221 229 40 176 65 50 209 92 4 14 121 249 183 21 31 188 101 0 48 152 128 40 203 135)
      #t
      ())
    #(124
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "HDE4W525s3TpJJmTmrD9fn7aRkVh66ifzXtHaYFKhjg")
        ("y"
         .
         "pHZM-M6XtdFDu47rnhsnKH8rc5QuzbxjWar7HuehUsI"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(252 143 100 234 193 199 230 136 197 44 70 113 133 222 33 145 78 139 37 48 86 217 228 190 1 14 208 18 143 146 168 137)
      #t
      ())
    #(125
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "qrz4sUQ9bLsd4Smg_-CfYLI_2dCkS2vfJb7XNz_b_R0")
        ("y"
         .
         "txa95_6fL0beC2iOMCXgKc_xUkRCmtT4NIT13qSvhYM"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(107 86 216 160 26 136 67 25 171 95 185 216 144 202 207 199 170 189 129 173 147 140 181 234 174 32 124 140 26 160 110 251)
      #t
      ())
    #(126
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "581YC9lXkV1ScFaDLjd5OrOwgt362TckEuGQjlwWu7Y")
        ("y"
         .
         "IIYBqXDVhEt4DZJG6Vg-s1kYxC7WlcB9UiRAN_DjHbU"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(47 100 181 200 4 109 65 164 225 214 49 255 35 132 107 255 149 106 73 37 164 127 133 52 73 10 32 180 177 145 139 156)
      #t
      ())
    #(127
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KlLbH-JGtxx5wNCsSafTjeZ7ICmV77vSqcxSX282AQM")
        ("y"
         .
         "aPSUvifgWT4tYS8foQqSEUN-aqFuZdl3NQFAcvDc7JQ"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(99 172 49 231 24 185 167 128 168 95 6 112 225 211 104 91 190 48 110 95 6 254 226 130 168 120 71 0 181 3 193 36)
      #t
      ())
    #(128
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "HFDcSf73CMTN1i52b5tg94TVGv7heo_p83AbL65Vt6U")
        ("y"
         .
         "0Q8NljnYPc6PJqhpcFptbTjm0yj1aFWBFCrsDc0fkOc"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(85 92 25 23 183 112 206 190 106 152 51 122 0 138 227 216 208 79 87 21 101 50 124 147 222 191 97 239 144 221 221 216)
      #t
      ())
    #(129
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "bQqhvBzubQfQRQAsEykNDKJco8h4M0OlJfrHBHK5LGI")
        ("y"
         .
         "1vunEXREi0cs8XKwyp43fxomA7p64SdtFTsgxj59JL8"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(58 101 169 32 15 143 150 99 89 18 250 165 231 133 159 163 3 167 106 28 42 65 234 151 239 97 170 57 40 119 0 169)
      #t
      ())
    #(130
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "8H49i-K6VMYIQUHh_Sspz9ANTm3W_7EV7YObEL2KQi8")
        ("y"
         .
         "QpksuaUkOJfVVAjpu1VgQzGNhzSa813MCXXtgFyPosk"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(187 123 181 45 165 112 186 88 224 95 211 34 248 45 85 108 45 101 179 101 219 48 129 88 121 246 127 35 59 8 155 81)
      #t
      ())
    #(131
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Q6m5AnTb1fNt0pBG_IOQAI3edFE85MPoiSsjbv_4DJ0")
        ("y"
         .
         "xxVHFSpYl9vhaVe9FdGofXcElvgU_ikhyPM98EOTx_g"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(232 202 233 148 66 51 184 103 238 223 89 2 252 73 236 208 126 76 129 196 98 121 83 30 137 82 11 116 186 83 112 181)
      #t
      ())
    #(132
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6a-OjBnanVwvOzwDuOknw8vi1xf5j1AJcuVtgusHwrE")
        ("y"
         .
         "ToP8qsrcJvi7XnuUdB_lTzEnXr1uHJadfsL-zq2KDa4"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(231 42 208 205 178 95 67 7 209 216 52 165 247 146 233 175 100 253 27 105 164 112 65 236 143 164 109 82 111 65 158 77)
      #t
      ())
    #(133
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "M9lYK1Z6rb5ZYG-m_8EYSOSUe1F5WXMXd2MXsrT_ZdA")
        ("y"
         .
         "tNhWjchDMZzAT0vxEElt7nySKfxoywlY88vTfsymmQ8"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(0 1 151 251 194 96 168 77 188 191 136 19 106 234 167 155 3 187 137 73 174 253 36 22 190 246 57 41 239 120 155 243)
      #t
      ())
    #(134
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "4hwCgq2xsgVf2nRGRMaGEs-wxopwuYEtAH8hp48a3Eg")
        ("y"
         .
         "SfPnZEvGYz4nc6LzzFIU-nII4wr7PemS8HfuMhVp3Eg"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(205 177 139 246 38 112 168 83 72 140 165 16 216 245 91 171 41 24 153 20 36 146 91 217 183 74 130 29 44 110 126 60)
      #t
      ())
    #(135
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ryfeDaZVbk5kWIyWlK_umoThy9DDiJct86mX92C7zZA")
        ("y"
         .
         "PFoC4WFVHzM9dwVZqxr0m_i2gnSJZZCTnOlW2ZE7Z28"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(22 115 3 80 93 34 207 158 247 140 91 150 135 165 65 143 169 251 40 79 43 15 246 131 22 40 142 205 127 46 46 9)
      #t
      ())
    #(136
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "DaQbglULNY_0dJFdgxBNQag6Eu9wWJudOS8PMNwyQp4")
        ("y"
         .
         "3HYWPI_gej9wnL2S2gu_xQRfPbgqpTRM8f1bJ_zS96Y"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(133 96 15 242 60 60 222 38 0 159 234 155 101 57 102 75 240 69 5 104 131 114 138 176 212 73 142 160 168 244 164 83)
      #t
      ())
    #(137
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "GchEuMcgkCagmWp4KYPhvQ8N6SVbhnOb6b7wjqVHXMY")
        ("y"
         .
         "aad53fV3R899miLwDtjvxugYr1gnt1DWZf7m1tWKIug"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(163 37 10 43 251 20 92 232 110 112 106 195 171 43 245 3 166 100 134 172 11 47 117 34 96 28 18 75 14 15 156 91)
      #t
      ())
    #(138
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "vQe9QybNyr9CkF76RVmjDmjLIV1Aya-2DOAtT9phdXk")
        ("y"
         .
         "uSe1y6AtJPuar-HUKTUeSLrp3ZLXvHvhXluKMKhr4T0"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(45 112 204 140 138 240 19 102 5 28 200 53 156 47 200 242 88 117 126 38 1 253 143 62 8 66 42 123 35 191 239 245)
      #t
      ())
    #(139
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AIne4npg0HHau69Y8-VmFNrTt_moAwdp_QRjs-bg8Do")
        ("y"
         .
         "FHtNbn5_2Tm5tU2rRY_VVq2P2vTabDkJWIxOBQynSmc"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(203 224 197 113 209 8 14 163 78 226 10 209 191 210 30 165 236 196 66 234 215 51 251 78 238 60 13 123 12 206 153 53)
      #t
      ())
    #(140
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Qu3hBs-FrvRt9-XbqKiwBFkxfZ52ant3wpmqDhfeoUI")
        ("y"
         .
         "tumob0_D6UXUMjuo5Fn2t7FMVjppjHV6LV97C8MB7eI"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(51 50 15 199 145 127 228 225 146 128 191 191 225 111 34 60 3 127 124 45 195 12 15 218 152 49 7 64 245 127 226 137)
      #t
      ())
    #(141
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "l0tDFsXn0TSLKNvE_WHY00cN50TDD1viN_hfKZad6nc")
        ("y"
         .
         "tfALWLg8_HvFFlVGW0ooq-HtPb7CDGtGQ67IW5WlvsY"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(53 199 38 234 214 108 57 65 79 224 194 70 4 223 120 56 229 114 93 47 193 189 8 83 38 30 29 227 51 142 203 79)
      #t
      ())
    #(142
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WYc9dSOTahIbYp6YcPkwQZ8lOldnudDcSXFvLFDhe9A")
        ("y"
         .
         "Fjtx8r9DGPveHOqlhUUAgO7ChHTNGL98IdLRv95P9nc"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(102 234 66 254 111 216 116 27 55 89 155 189 173 163 236 14 107 8 192 181 46 166 124 41 163 49 114 247 39 66 88 60)
      #t
      ())
    #(143
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "vYWnn4HE-WE-ZPo0eIZDeFbHNY0bac8ekj13Qtgvm2c")
        ("y"
         .
         "Z9JpGOqorLETodqtrtxwl0JFcwPrwjzdpVcmE9yCdwM"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(47 138 80 46 79 68 1 51 232 79 182 37 41 44 190 171 226 203 121 218 115 152 124 118 212 254 216 100 209 177 183 98)
      #t
      ())
    #(144
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "PmpO_8R8L1kmu2tKzy6sSLlSTEfVEfgWl2eWd4YA1sU")
        ("y"
         .
         "v85ZMkKlmFqXdZD410hd8_lTNSlX88F8E-lFg9nA57k"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(6 67 104 23 216 146 139 119 183 61 22 197 195 179 94 36 58 211 239 42 181 154 208 71 20 44 103 166 208 146 60 132)
      #t
      ())
    #(145
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "mkSH_PzoOWaI50SeCV_oA8qiU9S9fGbbxiYcydn4g6U")
        ("y"
         .
         "DlJRuuKcWlzfoxvGEQVnGoigGEZzmBWNNbiIKSN8C_8"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(126 131 253 44 61 113 59 200 93 109 133 217 7 139 58 8 66 130 77 65 14 138 189 224 77 160 253 113 199 217 71 5)
      #t
      ())
    #(146
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_tbOEnKQwSkcpc5krLTg8viQVlTR0lulfB90q1LyH0I")
        ("y"
         .
         "lj0xZxwGuAIWmSlSXEof3v9bHq-rkZ3C32xSvoTfrvM"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(14 61 253 171 96 110 189 198 66 130 130 172 212 67 241 137 201 155 59 72 58 161 1 253 141 107 237 56 174 197 158 2)
      #t
      ())
    #(147
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "987ltV8YafE33XB8j4-4llor5YQMMUn7dZaVpGYbnA0")
        ("y"
         .
         "I8eMTpZHsNbLLyYCvnP_Jc89CcltiStXRf5eyoFK7JE"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(244 137 242 189 147 247 107 142 65 252 107 159 33 27 197 153 212 157 177 241 122 56 233 91 171 29 49 178 162 181 88 41)
      #t
      ())
    #(148
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "K6quw7Po1UpOGPCWC5R9olNePPzKLPqLcROq2OO2Ym8")
        ("y"
         .
         "cvcefJ6WBCwdOcyPETnVFHxvT-YuI89t82S19NiZ-EI"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(204 87 56 180 157 48 213 208 44 247 224 197 74 61 224 155 91 111 60 77 234 145 221 6 121 7 42 53 98 68 76 55)
      #t
      ())
    #(149
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "pRqxI4vBvtJSR-fRecg6Ya4tSp_iKIw2OuDrenfeQyo")
        ("y"
         .
         "PG012CuoAX5sqQQcx4WjBwP3vEQnUG5iSsWXnXFUId0"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(137 161 17 119 214 144 122 129 212 116 103 9 59 246 163 204 139 165 93 238 5 35 155 22 10 49 163 0 15 93 128 123)
      #t
      ())
    #(150
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "i1rooOVfMPUJBhMVq655rEgPiLRGVfcmmjhcgVJohL4")
        ("y"
         .
         "Jil0oxoOIyISbC13smsQir2B-LlSxFjMyV1G-0kkx8A"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(44 176 60 48 178 0 55 165 207 77 91 51 87 79 58 186 200 149 191 171 55 134 126 178 235 237 38 14 9 41 5 141)
      #t
      ())
    #(151
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X2DHfkdN1myBNe49r8dbpkRkmCTHJzdUIJGtRprbtoU")
        ("y"
         .
         "MSwJxptinQQ2vzvWxgg_8qh75ISnPvOl0sPga12bIbM"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(229 77 72 125 12 75 18 254 82 42 243 230 99 206 49 110 99 43 169 214 58 31 2 163 111 197 168 43 248 39 49 164)
      #t
      ())
    #(152
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "4G6qc_b-rkVBfYWbutS8QEsohbzSE-us5ZThb0lw4MQ")
        ("y"
         .
         "Ee0zI6PXr8cHYjmIQwf5GEntX142thcdMJyBNExT4G0"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(204 234 150 157 64 250 66 147 63 79 189 196 202 190 33 133 248 164 82 153 98 84 193 244 224 221 229 225 79 238 234 141)
      #t
      ())
    #(153
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Dxwbien8b8D678kQn8ShJH2fVMdJe2zJdealRVvvQQg")
        ("y"
         .
         "Nss4GFSKybQeK4M2w-uNlwda5H4YJ_of-T1DQdQ8DB0"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(234 174 14 24 140 148 39 191 60 139 61 237 119 33 34 32 76 50 141 89 65 227 137 216 8 226 114 70 56 249 175 248)
      #t
      ())
    #(154
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "V3Bp6ChKlfUdyrkZsFNmVwWJcdq3Yhf4065yKmQJLiY")
        ("y"
         .
         "5R9opyLMA5f0gBQBdx6aPRmI1K928U-eL5w24Hc-KcI"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(254 160 204 225 53 143 31 244 15 254 170 255 191 145 178 232 212 38 212 227 30 150 39 115 26 206 58 18 46 171 107 13)
      #t
      ())
    #(155
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "JAaidZBQuSXdT4FMUDPjVVSPQrvxr7eRwRDwAx8p9oA")
        ("y"
         .
         "mdX0sAXeOSfxZavv8ZaijHIX-rG-K1IJwyTn1i0t1oc"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(131 118 33 234 72 39 187 160 55 106 170 138 166 108 254 20 74 47 241 227 89 220 97 154 6 68 29 62 5 95 151 113)
      #t
      ())
    #(156
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "zKrGHzWieGEYNiFkK8Vzr5EzVvtHz1gvC1KZCZ1vbGk")
        ("y"
         .
         "kfcnK4O3OKel0wRHyH8San2Y7HL6JgnQk50Y236n6zo"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(99 151 76 230 21 55 98 229 179 100 82 60 234 217 62 140 232 188 199 125 218 86 54 93 103 97 54 22 159 196 227 155)
      #t
      ())
    #(157
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AUFZFycvGYTnIXo2-zEf0pBNQaaxOXP5Kq47kOheTVY")
        ("y"
         .
         "2XyCLreyGoTQ0b5IZ0BKgMNIZ_QxOdrcw2GeELIiVis"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(10 4 136 20 75 195 109 105 11 98 20 138 195 7 96 71 212 109 72 247 173 187 15 52 254 233 166 54 41 95 231 55)
      #t
      ())
    #(158
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "slddEAxvoFa80TerERtTFaiQjCkkO4Tz3JltDkV2S5E")
        ("y"
         .
         "Zsq-tBiFWI7Ai0clffWL1Y99zZ4BLiZp-i9S4ldn_Ew"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(18 50 22 85 56 164 66 104 170 124 25 156 84 214 210 7 196 239 63 90 167 144 193 12 146 106 32 117 44 166 69 206)
      #t
      ())
    #(159
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "wXNV7TDM1kJ_loVwkCGyXBHtF26WEMR5vMTMdVKnOOY")
        ("y"
         .
         "H3URR2HboOxgzSZLurdjxdWrzHXNj7VlHQZFF5mIzG0"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(220 171 94 135 78 79 183 107 196 49 37 40 233 215 109 250 229 97 69 146 37 51 8 151 52 17 11 245 101 63 77 119)
      #t
      ())
    #(160
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "NBWSOQzM5IXeiIDz1yf2ZMOBkUob7Ow4OzVYZ1H8gcI")
        ("y"
         .
         "rdcYUrhwFuEBnK56kIDnXOCwuKrBddaS1ee02tCI9cw"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(76 226 112 27 43 230 58 0 131 164 197 63 122 11 240 76 248 113 101 79 94 219 111 98 94 62 165 231 208 189 204 144)
      #t
      ())
    #(161
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "-nZLa3aobDt2ISCCXTU6JHZiCMH1zA_j_nmYAmouxcQ")
        ("y"
         .
         "O7L5SP2UzapYabHg5zpNlwNcxJNX-3t01-0KLFuNVOs"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(122 190 249 118 92 202 114 19 32 251 248 237 203 239 109 43 162 93 23 183 15 250 23 118 2 155 195 143 230 119 161 44)
      #t
      ())
    #(162
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "px-7YXGZvVhbS2YhLKM8qeCTcOa_FcjqCs79nI6UXQY")
        ("y"
         .
         "hA8FiGMHjnQ-Ig_5nyO7wdqjaDXUsSafCnU25j8G2FM"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(95 97 64 77 187 188 40 103 223 249 92 31 55 237 68 244 203 143 171 205 34 59 3 115 157 136 131 8 209 59 196 18)
      #t
      ())
    #(163
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "E8gpLYVNOUUcDGOoArjAPk_Lh17wEjmJYpW6HA84aXU")
        ("y"
         .
         "-C3xlwhv2GAyyza2mieHbddajpZ582_8IhDtsSjUvhM"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(141 103 58 87 126 53 191 157 93 0 103 108 8 178 199 57 97 124 70 160 82 24 132 3 170 6 220 113 74 246 172 193)
      #t
      ())
    #(164
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "DNnfQVrMDDL9Tj1pJM5TB1sEUr-Rmiqy6-Jll1cPHs0")
        ("y"
         .
         "WYXY0sXfePwQD4fvtt-pVDdXvf_s8IPfzR7LON5sI_g"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(167 131 95 254 224 242 166 157 252 247 13 78 121 141 190 62 211 43 160 60 253 218 229 221 209 29 140 10 195 215 79 155)
      #t
      ())
    #(165
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "0tvqQEayP9KyM9HOMdzt24myXybAYnqdLbPFYFycyZU")
        ("y"
         .
         "Nb3I3nRRweJ-l6qRQCzOOILHEmnZy9y116wM65Ebm20"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(233 142 162 34 9 205 57 126 219 108 49 150 72 193 235 36 188 77 57 89 138 177 25 149 87 25 38 104 76 226 206 202)
      #t
      ())
    #(166
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "iI-wRPsrbKpgNmv6ZirbpHm4NlplVaKYh9WA9YcIa6g")
        ("y"
         .
         "SC9OwkCCpI1kAq-hYiFD8m5h2Rt-MNaksiNjDuEPcPs"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(145 182 87 51 134 11 27 219 149 65 217 245 88 149 163 219 179 241 60 25 146 81 211 48 6 182 220 249 10 195 73 237)
      #t
      ())
    #(167
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "LivsE0JJN51XcAMB86WOSzlaTSg3DSoG5l56yJ7XasY")
        ("y"
         .
         "l9yWC9eVzfT7z911FJBXuOAiMxx7VGHzg6xYnXZN8zM"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(31 223 124 92 72 4 122 17 62 94 93 27 126 213 147 51 126 118 146 49 204 165 199 17 1 96 224 193 185 127 66 86)
      #t
      ())
    #(168
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "x4zafjueF3Lr7TCytR3PFVppoPxQRVeDbiUUfPuBJ9I")
        ("y"
         .
         "-Cic84sDPTdjyPn2wJF4ejFC-4Pf9XGVkCgsb4UuAQU"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(186 10 188 62 113 114 108 181 19 48 72 145 118 53 123 129 184 7 77 118 144 228 232 46 154 60 0 21 30 31 163 24)
      #t
      ())
    #(169
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Hj303X-3cYywqg3XL4olyDxOgE58vUjF6WVlH54jv04")
        ("y"
         .
         "8P9A3ZeW5Kml7d0sTKTr0QmQ2PuJGNEtU8dgAa-p3n8"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(22 230 50 249 117 45 54 96 44 149 236 39 75 50 173 89 79 57 246 172 59 212 176 178 15 134 55 57 33 66 206 244)
      #t
      ())
    #(170
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5cXcP9iNhWaLO3Cf1rQjLx-AlJy8y1WINj5sIXorPtg")
        ("y"
         .
         "jb0NbjzJfzCB0WYCqj0bZV7geRyH_LWr5iF9jIUTgH4"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(158 237 75 150 86 159 96 74 77 63 90 249 116 153 128 113 17 252 152 136 196 88 236 226 227 0 14 36 92 44 2 176)
      #t
      ())
    #(171
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AhxB7O7CTg-6iUrXQVqVmMvNFPpspG4lV1Joodjlu8Y")
        ("y"
         .
         "P4Rsahhfo_I7uSwU5-LLqMdAR8Ca92b1XvDJB8gNlFE"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(33 172 50 1 56 56 129 38 33 219 181 132 150 91 222 214 252 133 29 58 2 152 16 103 155 197 123 35 129 187 122 125)
      #t
      ())
    #(172
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "jiQZLNMzNaEU9QcCZsAUyw2McE0W1gQuicF1l7zU534")
        ("y"
         .
         "vbTFFxcEwsCSdcIqMQ4MT-CS5AhIVtqZuUq7-p9Gn0g"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(252 89 120 218 1 202 131 225 39 221 223 152 154 3 88 135 27 60 76 224 117 91 251 2 6 51 219 70 126 33 165 60)
      #t
      ())
    #(173
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MckK5HqT0JojUrbzZ355depiqt7bVsEY64ufdx4t2fU")
        ("y"
         .
         "8mAfucyiME5ZRCPPSAZNvtF65ARS8YvmrgGDIZEejLM"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(159 65 115 65 38 26 164 93 57 107 12 207 42 61 238 122 70 108 164 126 60 232 110 205 32 113 217 196 219 8 130 14)
      #t
      ())
    #(174
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "0vIRz6uE4ByOVUQDYjTevjWuEDu4eNerzqaCX3U-A6M")
        ("y"
         .
         "hffxhw5k8SYq9nol75iAQZ9FYI5_nabe6D9fRs61Pcs"))
      #(("crv" . "P-256")
        ("d"
         .
         "gJxGHYs5FjU3_49e9bl35M25gOcOOKfuCzfMh2cp6f8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BGiudwYiHlmQ90hNNPvsWpkFAXmmwRgXu-1K7ZYpmP8")
        ("y"
         .
         "tSKNiaG0SPEjMjdsjH8IB2NTKgVeB_FKXeDcMBBFeeE"))
      #vu8(244 25 254 187 50 194 84 97 26 223 86 156 45 88 59 23 84 43 21 56 202 160 0 25 103 240 164 188 52 184 183 137)
      #t
      ())
    #(175
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Xsvk0aYzCkTI9--VHUvxZebGtyHvramF-0FmG8bn_Ww")
        ("y"
         .
         "hzRkDEmY_343SwbOGmSi7NgqsDY4T7g9mnmxJ6J9UDI"))
      #vu8(133 160 181 133 25 178 142 112 166 148 236 81 152 247 44 75 253 171 170 48 167 15 113 67 181 177 205 117 54 247 22 202)
      #t
      ())
    #(176
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "AAAAAP____________________________________8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "acDnZ_m-qr_bzfMEoOxotfbWcQPPN1if2EbaGh3UR7M")
        ("y"
         .
         "xRdBbkUBRGP8nPHEBH0CrORdpHORaqUdey3D0IAzRSs"))
      #vu8(163 41 167 216 4 36 234 45 108 144 67 147 128 142 81 13 251 178 129 85 9 47 27 172 40 77 206 218 31 19 175 229)
      #t
      ())
    #(177
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "AQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "m78G2tmrWQXgVHHOFtUiLInCyqOfJiZ6wHRxKYhfvUQ")
        ("y"
         .
         "G8x_qE3hIKNnVdrzCm9H6MDUvdwVA27So0R9-nodPog"))
      #vu8(189 38 208 41 62 136 81 197 30 190 13 66 99 69 104 58 233 64 38 172 165 69 40 42 71 89 250 168 95 222 102 135)
      #t
      ())
    #(178
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "f_________________________________________8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "wdFyaeRuOHrL4pnsLMnMLa2j8F5M9BLyrZRrcAqiYTo")
        ("y"
         .
         "7bd0TzcME6T0mVfVT_eYEZ0RH2kSnCTbX1-4QWKQnbs"))
      #vu8(234 147 80 178 73 10 32 16 199 171 244 63 177 163 139 231 41 162 222 55 94 167 166 172 52 255 88 204 135 229 27 108)
      #t
      ())
    #(179
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "gAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "d7IKkS5rIxNQZukRiRUkvE7-NWDj6SNQtS3sjzdfK1Q")
        ("y"
         .
         "o9wpGCXOo_f3sQv83QOKct9iPaHoUODxyqgB_NbMZ_8"))
      #vu8(52 238 211 246 103 61 52 11 111 113 105 19 246 223 163 107 90 200 95 166 103 121 30 45 106 33 123 12 11 123 168 7)
      #t
      ())
    #(180
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "_____wAAAAD__________7zm-q2nF56D87nKwvxjJVE")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "D6givCgRqqWEklkuMm4l3ilJO6qtZR9-kOdctI4U22M")
        ("y"
         .
         "QAu1FgokV_OQtStDTCDndMtOWbCvAX0KG-7bq6C50Rg"))
      #vu8(19 84 206 102 146 201 223 123 111 195 17 157 71 197 99 56 175 190 220 203 98 250 165 70 192 254 110 212 149 158 65 195)
      #t
      ())
    #(181
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "_____wAAAAD__________7zm-q2nF56E86nKwvxjJVE")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "VMzJQVAm1z8gqEW3KljlsYvSfxmFQqC-7qa8kgceXIM")
        ("y"
         .
         "47zAuUuuus7FcHjqJS1A3W1h9DSicRtpMC9xCOv26V4"))
      #vu8(254 116 150 195 13 83 73 149 240 191 66 139 84 113 194 21 133 170 175 200 23 51 145 111 1 101 89 122 85 209 44 180)
      #t
      ())
    #(182
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "_____wAAAAD__________7zm-q2nF56E87HKwvxjJVE")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "yQ4wCDndWJUegJVwYxVEA8tOZkTHdDZIE9AMJI-o7kE")
        ("y"
         .
         "nQr755LmGMRj-O85rHv8na-6rLnoUzKRuFnJmDwoPlE"))
      #vu8(52 139 248 4 46 78 223 29 3 200 179 106 184 21 21 110 119 194 1 183 100 237 69 98 207 226 238 144 99 143 254 245)
      #t
      ())
    #(183
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "_____wAAAAD__________7zm-q2nF56E87nKwfxjJVE")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "f-NrQK8ir4khZWsyJixx2hq5GTZcZd-2OlqeIhhaWUM")
        ("y"
         .
         "GWgrptpJydxg9gv7-CNZDotMKnqEdQ3tKvLq05ZjXv4"))
      #vu8(110 78 197 71 154 124 32 165 55 80 23 0 72 79 111 67 58 138 143 229 60 40 143 122 37 200 232 201 45 57 232 220)
      #t
      ())
    #(184
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "_____wAAAAD__________7zm-q2nF56E87nKwvxjJPM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gNGZfr2-jlTgVZYZbHqV7DpT4me6KiRxmWCqV35hUOQ")
        ("y"
         .
         "DGlrAUfbBllbJyhKDBZSTAER6Y1KRlilq7k4531Xgu0"))
      #vu8(247 64 125 97 253 245 129 190 79 86 70 33 213 144 202 155 123 163 127 49 57 97 80 249 146 47 21 1 218 140 131 239)
      #t
      ("AddSubChain"))
    #(185
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "_____wAAAAD__________7zm-q2nF56E87nKwvxjJTM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "QJ-NohrqI2pfWhkE0DEMHGGSpn0NoIk2MZhpqK0IOKM")
        ("y"
         .
         "jyMITTD_canx2RjULEK_CMFmXxTp1ZhsHpwtONXhcKU"))
      #vu8(130 35 111 210 114 32 134 147 224 87 69 85 202 70 92 108 197 18 22 52 134 8 79 165 127 94 27 210 226 204 192 179)
      #t
      ("AddSubChain"))
    #(186
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "_____wAAAAD__________7zm-q2nF56E87nKwvxjJUM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "VOd6ABw4Yrl6dmR_QzbfPPEmrL56BpxeVwkncyTSkgs")
        ("y"
         .
         "CmYOQ9YLzou97eBz-l0YPI6OFYmMr2_35Fg30J8vTIo"))
      #vu8(6 83 113 73 102 77 186 26 153 36 101 76 183 247 135 237 34 72 81 176 223 37 239 83 252 245 79 143 38 205 95 63)
      #t
      ("AddSubChain"))
    #(187
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "_____wAAAAD__________7zm-q2nF56E87nKwvxjJUs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sBoXKnakYCyS0yQsuJfd4wJMdA3rshW0xrCq6Twikak")
        ("y"
         .
         "F6Pvis3IJSuQE_HSBFj8huP_CJDjgelCAoO3rHA4gB0"))
      #vu8(242 179 133 57 188 233 149 212 67 199 191 238 239 173 201 228 44 194 200 156 96 191 78 134 234 201 93 81 152 123 209 18)
      #t
      ("AddSubChain"))
    #(188
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "_____wAAAAD__________7zm-q2nF56E87nKwvxjJU4")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Xsvk0aYzCkTI9--VHUvxZebGtyHvramF-0FmG8bn_Ww")
        ("y"
         .
         "eMub8rZnAILItPkx5ZtdEyfVT8rHsEfCZYZO2F2Cr80"))
      #vu8(133 160 181 133 25 178 142 112 166 148 236 81 152 247 44 75 253 171 170 48 167 15 113 67 181 177 205 117 54 247 22 202)
      #t
      ())
    #(189
      "edge case private key"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MQKPM3f8jysZZ-2quQITrK0NqfUIl_CPV1N_ePEWdEc")
        ("y"
         .
         "Q6GTAYk2O73irEy9FknNxvRRrdcd0vFqioZ_KxfKoWs"))
      #(("crv" . "P-256")
        ("d"
         .
         "_____wAAAAD__________7zm-q2nF56E87nKwvxjJU8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "fPJ7GI0DT36KUjgDBLUaw8CJaeJ38hs1pgtI_EdmmXg")
        ("y"
         .
         "-Iiq7iRxL8DWwmU5YIvPJEWCUhrDFn3WYftIYt2HjC4"))
      #vu8(2 123 1 58 111 22 109 182 85 214 157 100 60 18 126 248 172 225 117 49 30 102 125 255 37 32 245 181 199 91 118 89)
      #t
      ("AddSubChain"))
    #(190
      "CVE-2017-8932"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AjgZgTrJaYRwWQKOqIofMN-83gP8eR06JSxrQSEYguo")
        ("y"
         .
         "-T5K5DPMEs8qQ_wO8mQAwOElUIIkzbZJOA8lR5FIpK0"))
      #(("crv" . "P-256")
        ("d"
         .
         "KiZfi8vcr5TVhRkUHleBJMtA1kpQH7qcEYR7KJZbxzc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "HAB87s8hVghneih_jnC6btXARlhEvYjJ41FxMS80aKY")
        ("y"
         .
         "DguxnlM-tMEWK9qvywU0g7HAdZ-M4DYPw8iM0H1Q6rk"))
      #vu8(77 77 232 15 21 52 133 13 38 16 117 153 126 48 73 50 26 8 100 8 45 36 169 23 134 51 102 192 114 79 90 227)
      #t
      ())
    #(191
      "CVE-2017-8932"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "zBGIey1my66PTTBmJxklIpMhRrQvAdPG-SvVyLpzmwY")
        ("y"
         .
         "ovCKApzQa0YYMIW66SSLDtFbcCgMfvE6RX9a84JCYDE"))
      #(("crv" . "P-256")
        ("d"
         .
         "MT9y_5_oEb9XMXYjGyhqO9tvGxTgXEAUZZByenHDvM0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WgYrCxiSExev-7PULD2_yrKWy_IwQaYAggTDuDHG324")
        ("y"
         .
         "YGZ1nbACvlovLprkgNokiwsqmTAtWS3lUpPRY0EkY9Y"))
      #vu8(131 28 63 107 95 118 45 47 70 25 1 87 122 244 19 84 172 95 34 140 37 145 248 79 138 110 81 226 227 241 121 145)
      #t
      ())
    #(192
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(193
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(194
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________4"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(195
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________8"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(196
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(197
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(198
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE")
        ("y"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________4"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(199
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE")
        ("y"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________8"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(200
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________4")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(201
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________4")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(202
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________4")
        ("y"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________4"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(203
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________4")
        ("y"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________8"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(204
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________8")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(205
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________8")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(206
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________8")
        ("y"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________4"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(207
      "point is not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________8")
        ("y"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________8"))
      #(("crv" . "P-256")
        ("d"
         .
         "fkqlT3FL8B34XFAmm-o6hnIfhK_nT3tB6lirzzR06I0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R_Z9415ctDKwV-ig0P9iLraYs0vJ_-UjURk2Y8xucnQ")
        ("y"
         .
         "FA-BXvYf9u0G3woVA2bqvPsY7aHO49SzDy0V1KT65Rc"))
      #vu8()
      #f
      ())
    #(208
      "public key has invalid point of order 2 on secp256k1.  The point of the public key is a valid on secp256r1."
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____________________________________v___C8")
        ("y"
         .
         "PZ323vrKMCmFBhE9dQGXhQfTOf4FR5YsrEzW38Vsl2k"))
      #(("crv" . "P-256")
        ("d"
         .
         "_bpUs8It41jZN0MbFItKUqMUQmr9rFUMqowMBM8LqvQ")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "onTWzhAYDTp_B1ksqqMYZOmmkOq49pFLVc55WfIn-DM")
        ("y"
         .
         "LUbjS3eaeIGm6CQuQc863-ro19IMT-JKqoAHAfA9sRQ"))
      #vu8(193 205 152 108 225 97 180 241 0 14 233 229 251 49 36 91 171 237 213 73 158 98 165 229 99 189 215 26 136 248 147 22)
      #f
      ())
    #(209
      "public point not on curve"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "FRAmTBicPVI_-ZFqvXBp76aWjY3H3bZFfXhptT6mDNw")
        ("y"
         .
         "-vt-1HhtoV0p7lklb1Nto1daSIjBuwqVslb0p-n9dkw"))
      #(("crv" . "P-256")
        ("d"
         .
         "TzQU0VibSfcXLUOcu-eOW1NQ3IXepAzS1idHQMbgI5w")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Qd7jCiJE2SAcpk1v0t0Sa3CVNp5nOuyqpeBCMIUonV0")
        ("y"
         .
         "TRc1uShB5308iyBk3tLD1l2GkvCgVFJl322i5SuUock"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(210
      "public point = (0,0)"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-256")
        ("d"
         .
         "TzQU0VibSfcXLUOcu-eOW1NQ3IXepAzS1idHQMbgI5w")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Qd7jCiJE2SAcpk1v0t0Sa3CVNp5nOuyqpeBCMIUonV0")
        ("y"
         .
         "TRc1uShB5308iyBk3tLD1l2GkvCgVFJl322i5SuUock"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(211
      "using secp256k1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "oSY-dbh64JNwYP8UcvMw7lXN-PQynWKEqev7zIVsEWg")
        ("y"
         .
         "QiXnLL6_9B5U-28A4Rr-U6F5N77b8t94f475WE93WDg"))
      #(("crv" . "P-256")
        ("d"
         .
         "TzQU0VibSfcXLUOcu-eOW1NQ3IXepAzS1idHQMbgI5w")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Qd7jCiJE2SAcpk1v0t0Sa3CVNp5nOuyqpeBCMIUonV0")
        ("y"
         .
         "TRc1uShB5308iyBk3tLD1l2GkvCgVFJl322i5SuUock"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(212
      "Public key uses wrong curve: secp384r1"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "HxeQHnMbBvNJtunX0X1F6KK0YRWkdIW-Fhl5MtuHs5QFtclBs2_WG5733SCHjhKe")
        ("y"
         .
         "VaIncJnGAdzbN0f4CtbhZhFjeOHrziyVdEoJhhKM_uqsf5C3F4fZoc_kF81Mj2r1"))
      #(("crv" . "P-256")
        ("d"
         .
         "tE-WcP7bqIetjoBiJgY-d2BLJ8Nig2Mm6T7Lf8xtwpc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "n1e4_HBpzkbKmlbmPYjpnU7LWEX4HnHPa8hqzIWL0pA")
        ("y"
         .
         "iGn8nuVhfbtkaK82OB8MQJaNdHlOGDpmmwGS69nXVhE"))
      #vu8()
      #f
      ())
    #(213
      "Public key uses wrong curve: secp521r1"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AO125YiEKPrUCf8gOrKYsPJIJ8CRk5rg-bEkXYZaxfvNJ0n5rmyQ-o4pQU0bx9x7PErKkEzYJEhEIcxm_mr0O9_S")
        ("y"
         .
         "AMH3kKCzrplJN_kba9uXeLCMg-ytuMuiKnjDe_Vl2sFk8Y5xm-DviQ7ly_IOF_z8mlWF5UFkcLmGL4L7dpM5mU9O"))
      #(("crv" . "P-256")
        ("d"
         .
         "tE-WcP7bqIetjoBiJgY-d2BLJ8Nig2Mm6T7Lf8xtwpc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "n1e4_HBpzkbKmlbmPYjpnU7LWEX4HnHPa8hqzIWL0pA")
        ("y"
         .
         "iGn8nuVhfbtkaK82OB8MQJaNdHlOGDpmmwGS69nXVhE"))
      #vu8()
      #f
      ())
    #(214
      "Public key uses wrong curve: secp256k1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gCjRYIKwdpbUqkqrnWsfFGNDWsCXkAYxEI-YiOE9pnw")
        ("y"
         .
         "SEH9jdPO1uetjG_GVmIcL5PT2w6ynUjRQjFUUZhl28E"))
      #(("crv" . "P-256")
        ("d"
         .
         "tE-WcP7bqIetjoBiJgY-d2BLJ8Nig2Mm6T7Lf8xtwpc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "n1e4_HBpzkbKmlbmPYjpnU7LWEX4HnHPa8hqzIWL0pA")
        ("y"
         .
         "iGn8nuVhfbtkaK82OB8MQJaNdHlOGDpmmwGS69nXVhE"))
      #vu8()
      #f
      ())))
(test-ecdh-jwk
  "ecdh_webcrypto_test"
  :algorithm
  "ECDH"
  :curve
  "P-384"
  :encoding
  "webcrypto"
  :tests
  '(#(215
      "normal case"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "eQpuBZ75pZQBYxg9SngJE10peRZD_EOi8X7ov2d6uE95G2SmvhWWn_oBLdkYXYeW")
        ("y"
         .
         "2blUuqinXoLfcRs7Vurf9rD2aMOya0sa6zCKH8wcaA0ymmcFAl8cmKC15b_LFjyq"))
      #(("crv" . "P-384")
        ("d"
         .
         "dm5hQlstqfhGwJ_DVkuTpvhgO3OSx4UWW_INqUjEn9H7He5O3WQ1a58hxYi3Xf2B")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "em7I0xHVyliLrtQb4-mPMMkpSETsu2KZlWU2NdvCLaLwg_KXEeD5xZY7wCG9jLIQ")
        ("y"
         .
         "na9WpV-IOnIAzqnE3kRIjm3En7nDlPUctaSfxp1-igNHkpY65Oq8Y0g6LPGomejI"))
      #vu8(100 97 222 251 149 217 150 178 66 150 245 161 131 43 52 219 5 237 3 17 20 251 231 217 141 9 143 147 133 152 102 228 222 30 34 157 167 31 239 12 119 254 73 178 73 25 1 53)
      #t
      ())
    #(216
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SQ6W0X9MbOzNRd70CM6jPpcEpfGwGj3i6qo0Cf0WDXjTldazsAPXH9H1kPrZW_HJ")
        ("y"
         .
         "2GZe_CBw0FmqhHElwvcHQ1lVU1x8XfbWwHnsgG3Oa2hJ0zcUDbfKUGFvlFbeEyPE"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(217
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AWEyiQlnUhPjIJjTWmuDCKjVAMyjnc7l6ATnO9uN6vBv5BcpH9l5OyMe9f6GlFRE")
        ("y"
         .
         "qXoB8646gxDEr0m1ksspHvcO5bx_VTTTwj3J7v3iMEhCx3N66TfM-b0hXCgQPp_i"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2)
      #t
      ())
    #(218
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "r0rpZOO8vZI6zNpdoxddQR_WLRfdPDocQQvvFzCYWmJl2Q6VCsD8UHQ7HtdxkG_z")
        ("y"
         .
         "O2jPTT2DqIWocJf90ynOg7GJ-YzsW-RMMdGjoruhD0cZYyMri6dhD6jHIXkFDrht"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3)
      #t
      ())
    #(219
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "FHirbgMrlUXtqawsJk5XoR8IrLx20WoKt3sE29ryDyFcQYNDezKvxHHqpgPRTHxd")
        ("y"
         .
         "ikyE7g6JW-xcN_ChygdeEG_2vziAG1xpdAnTlnUjEQjTPEpeplqqjAPpOcldlsTE"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1)
      #t
      ())
    #(220
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "9jII405-kLtfsDZDJGeomYFEQBBmO4UztHv6lL0rwW84qlFrkwpHJuOHbTCRv7cu")
        ("y"
         .
         "x4PtTaDKwGMggX3IvGT1nM8G9Iq8Q4ahUJE_qVdDp7RgEZDhxu6Pi_Y1SyVOys5F"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255)
      #t
      ())
    #(221
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AzJx70LZKtR7JzsJ6i9FQBFhuqUmllkNDhdf8tHA36P-pA5CZtRGVGwF5IDVf6vs")
        ("y"
         .
         "eInxaovMF2YC9tRlYWFKL0KEq-aXt8uc559-LnGxVcsfFVzpJdFjkaaA7aIxUubh"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255)
      #t
      ())
    #(222
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "c36EN-GGg94kVbaJRbujHa7D51TXLwoHdtMZKy-SmLuVyhRkuqZoequ2efgEz27G")
        ("y"
         .
         "wrTUfWGmBATfY7HprAlUs0GbvCrVKgQJruuC9HA3WFiAWRZbIDZ9y0sjWwyvcdcn"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(0 127 255 0 1 255 252 0 7 255 240 0 31 255 192 0 127 255 0 1 255 252 0 7 255 240 0 31 255 192 0 127 255 0 1 255 252 0 7 255 240 0 31 255 192 0 128 0)
      #t
      ())
    #(223
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "N_kASYMVa72cR4kedSN7sTAWvX_m9OD3HO8OY_FqZy8NOw4gFlwzQH4Ua2pK5pYt")
        ("y"
         .
         "07V8y5nnqvEwMkBRbQ6-COWFUT42ldQsRn3KtTQO92GZDK3I2IQKrMlESBQVwH_r"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(59 94 237 128 114 123 203 197 17 59 138 158 77 177 200 27 29 221 194 217 159 245 109 156 60 16 84 52 137 19 189 226 150 49 28 75 210 250 137 155 77 14 102 170 161 182 160 221)
      #t
      ())
    #(224
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "llXY5WInGLMXz7wJiUNX91prE_pRa81mMHIbhppiAZbPDD3siGCzLSftm6ws8mOv")
        ("y"
         .
         "FzIWmBFtfYEa6NqbnLv5OCweNuK2fWxq-bzqfZ3gDKcrOYYGwJigoPDEuJQZQ-1l"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(106 153 165 172 212 167 237 177 199 7 215 248 190 18 232 17 64 51 142 62 20 186 86 60 112 60 104 26 49 154 63 156 225 249 15 3 43 248 64 243 117 142 137 203 133 44 236 166)
      #t
      ())
    #(225
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "zLE9Qns8S7M91PIM3avGhgDq-X7rLIHowhiukHQ-dP84ylbwwCJDedtGTc9KQPBD")
        ("y"
         .
         "UM16ZZssSFGl3PjJkPySDAfU1apQohhXUOa4TELoPP9jUFBILey0eA-BLkxJ_HQE"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(124 37 164 245 127 118 171 19 178 92 171 60 38 93 185 217 189 146 95 236 191 123 249 59 239 19 8 119 134 70 98 141 236 171 6 126 217 136 169 117 92 216 142 136 222 54 113 4)
      #t
      ())
    #(226
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "JmRiQwfALvSHAwpjIWLFFfhB0V6jFS2Y_yNkIy16qzk0PV9wOk1aMQkqpzVsOi9n")
        ("y"
         .
         "HBzWA63f2LVHdVKjsyoY7a8-M77CLuIWf52nKWNgAqeXTq61_wgrKqv4xwVrhMOr"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(127 255 255 224 0 0 15 255 255 252 0 0 1 255 255 255 128 0 0 63 255 255 240 0 0 7 255 255 254 0 0 0 255 255 255 192 0 0 31 255 255 248 0 0 4 0 0 2)
      #t
      ())
    #(227
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Zl8fMgtqscG1LRROUth6FUwrRImDjJEZ3mIsLRtStlsKOVXkTg1IWRdTYMD2Pe6B")
        ("y"
         .
         "PxT2mXLxjK7XkWyUpNIOw0RZHnU2pKek2MmDKBjJbWCxqB-r5k6gLF9kfjYb9bYP"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2)
      #t
      ())
    #(228
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "kTV8qH27COhdexrOz9HghgeKgtGfgUdNo4k2Sjn-JUPrk0tEAXPDjmGh2UB4VbXY")
        ("y"
         .
         "nvDZ6SB2S213ZbCEz5VB2sxD0dq6o5Cw-4Vgl7DACoVW9OOEhWirSueQw9NGygG2"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(255 240 0 0 0 31 255 255 255 192 0 0 0 127 255 255 255 0 0 0 1 255 255 255 252 0 0 0 7 255 255 255 240 0 0 0 31 255 255 255 192 0 0 0 127 255 255 255)
      #t
      ())
    #(229
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "1agzuuM7LRD9_223xUd622FLGRxw2XxvEwoU6TkxzB3AWAU_7lSiZKAP3RbTFm_c")
        ("y"
         .
         "QpkidreZJbr80YOwPtGCNTUJgKv-Z7gUxsEQdMOPdM1Oc0rVjNtJ2fzSGB0bjxEZ"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(255 255 240 0 0 3 255 255 255 0 0 0 63 255 255 240 0 0 3 255 255 255 0 0 0 63 255 255 240 0 0 3 255 255 255 0 0 0 63 255 255 240 0 0 4 0 0 0)
      #t
      ())
    #(230
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Z1R82n--jxa-WkR3y7ApefGvcvwPOTAnc1UvvPRmeo4jq8DhKFbuYjTe7KXyKuBQ")
        ("y"
         .
         "Ok33wGjnQyQXJgy5_g1oucf89-FqKtoFaH2PiQC4RyMQPtv_CkKydRfaJ2C304hD"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(255 255 255 0 0 0 3 255 255 255 240 0 0 0 63 255 255 255 0 0 0 3 255 255 255 240 0 0 0 63 255 255 255 0 0 0 3 255 255 255 240 0 0 0 63 255 255 255)
      #t
      ())
    #(231
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "E2PjuZAI4Juz8IWUm5tuomoxj0lt5WipZjD9udTHLCgU3zCHoXQfMvJJibQoFn-T")
        ("y"
         .
         "xlPLOujD7PrsV-_VS7jOnXnHv2zHD7ERT5Ob6PGpm_HkK5dDESTvn6M0UPqk52g5"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(255 255 255 255 255 255 0 0 0 0 0 0 0 255 255 255 255 255 255 255 0 0 0 0 0 0 0 255 255 255 255 255 255 255 0 0 0 0 0 0 1 0 0 0 0 0 0 1)
      #t
      ())
    #(232
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "uivo1xR-JBfC7ICyS0waqURk_9Cq4fouB4s6-8d8FESJyp0GSsu3qc-mGW0PRnt-")
        ("y"
         .
         "Ze4coesTUf-ZaPVT3-LkxZ_4ujTCKkKzuqE6mhrcfxOr1A8f0l1GvFMwhSuTcZZq"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(233
      "edge case for shared secret"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "1phQzLrMRzbqIA_y-EiPJiR5RaKrSN03CPSUspPYy6g0F_SJdIgcf7A4VAibv2bM")
        ("y"
         .
         "HHc-wDy4zV8AfsOwO90FpAmzUhA_DezyW0FnOrjKPQQzS6vuASGfFXAfK8oi1As3"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 254 255 255 255 255 0 0 0 0 0 0 0 0 255 255 255 254)
      #t
      ())
    #(234
      "y-coordinate of the public key has many trailing 0's"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "b8r4LZgtIi1glrqD5VscfctxpB6I8yMzP0QoTZXEvTYW2nob75KPMcJviFunrbSH")
        ("y"
         .
         "gm_eLtn1ZJwRz4Rl-L-K1Q9okUk2_Dlmb2ghnQZlBr6kAB_cgWyakOfir7Gb6ghf"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 31 3 18 59 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(235
      "y-coordinate of the public key has many trailing 1's"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "b8r4LZgtIi1glrqD5VscfctxpB6I8yMzP0QoTZXEvTYW2nob75KPMcJviFunrbSH")
        ("y"
         .
         "fZAh0SYKm2PuMHuaB0B1KvCXbrbJA8aZkJfeYvma-UBb_-AifpNlbxgdUE9kFfeg"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 31 3 18 59 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(236
      "y-coordinate of the public key is small"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "v-tH-0CmWHjmtkL0C44VAirens-oy2GAQwY0lOK8XS3xDTbzeGm1jvEtzDXjmCg1")
        ("y"
         .
         "_S5V7EH9_oyru7e82BY2RaGenaxZYw8_6TsggJT_h81GG1PO9TSC5w4ujqhyAMw_"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 54 162 144 124 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(237
      "y-coordinate of the public key is large"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "v-tH-0CmWHjmtkL0C44VAirens-oy2GAQwY0lOK8XS3xDTbzeGm1jvEtzDXjmCg1")
        ("y"
         .
         "AtGqE74CAXNUREhDJ-nJul5hYlOmnPDAFsTff2sAeDG55KwwCst9GPHRcViN_zPA"))
      #(("crv" . "P-384")
        ("d"
         .
         "orZEKjf4o3WdLLkd9eynWxT1pnZtqANcwZQ7Fajk67YCXzc74zQIDyKrgho1Naan")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X7b8XxfVh97l6VAsTEiO89awbAmBAvYjCiZNC00ZJ8RhYB5HyrbZP68O6YIp1cXd")
        ("y"
         .
         "tBbIpSH978yrXL0Uo5sFxriMtl_7SHkIYlI1xpFkw55eJ-w0fH9Y-DEa3kIDSozh"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 54 162 144 124 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(238
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "PPme8E9RpepjC6P5-WDdWToUyb45_SvSFdO0sIqq-Gu_kn8sRuUqsG-3QriFDlIe"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(96 146 161 117 125 221 67 160 78 24 95 249 71 42 13 24 199 247 167 220 128 47 126 5 158 12 105 174 22 200 2 101 23 25 64 110 4 222 39 101 47 248 61 164 167 128 239 47)
      #t
      ())
    #(239
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAC")
        ("y"
         .
         "cyFSRC-27lw-bOHZIMBZvGI1Y4FNeQQrkDzmDx1Eh_zNRQqG2gPz5u1SXQIBe_2z"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(137 200 4 203 129 68 51 134 177 133 188 217 226 230 195 94 230 23 124 59 144 41 137 133 196 232 26 137 213 32 204 235 23 215 41 84 14 86 236 195 67 194 107 243 20 242 208 82)
      #t
      ())
    #(240
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD")
        ("y"
         .
         "ZmAEGxx5hGIOjX_XzNtQzDuoFtoU1BpNiv-rqEiIZ_DKWiT41C3X5EtTCifcW1ja"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(53 81 49 87 232 4 189 145 141 4 222 32 39 120 184 26 111 199 173 138 165 65 238 148 17 106 15 24 70 103 37 215 94 113 198 148 43 240 68 177 176 236 186 25 219 51 224 222)
      #t
      ())
    #(241
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAD__________wAAAAAAAAAA__________8AAAAAAAAAAQAAAAAAAAAB")
        ("y"
         .
         "FBue5TEOqBcBMbYESEptZ37UJXYEW3FDwCZxCukrJ3r7vqDERYwiDVYeaUBNx9iI"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(16 32 128 192 71 136 29 25 174 251 1 194 156 130 164 251 50 138 142 166 230 214 201 20 175 115 16 5 7 200 238 73 151 153 170 166 70 222 14 168 194 114 124 11 94 210 67 155)
      #t
      ())
    #(242
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAP____8AAAAA_____wAAAAD_____AAAAAP____8AAAAA_____wAAAAD_____")
        ("y"
         .
         "cDcDhUE9Pv9vo0B7ok9oLCsBtRRF299e97DdCXnxfnE-CQgVcfHpTftmvyggAvOf"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(246 137 246 228 117 180 225 81 98 82 26 202 180 99 122 60 219 156 180 42 169 47 145 20 176 238 48 13 218 232 157 94 175 255 52 99 161 245 0 74 42 27 212 174 255 164 123 120)
      #t
      ())
    #(243
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAD__wAA__8AAP__AAD__wAA__8AAP__AAD__wAA__8AAP__AAD__wAA__8AAP__")
        ("y"
         .
         "ES4ZHx94u8VLbMTwseWa6Mb_Ggf1Eo5B36KCjhtlONT6LKI5TGqrNEncs_xOtEwJ"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(243 72 98 68 17 155 54 50 253 85 190 158 105 81 235 93 156 140 98 246 162 112 66 249 75 146 65 85 236 253 79 248 116 75 163 210 91 207 133 167 185 37 189 40 161 43 137 127)
      #t
      ())
    #(244
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAIAA")
        ("y"
         .
         "AopMjaWgURL-YCXvQZCJad4g0F2WaOXIUu8tSSFy3cKgpiL8SIFk_MGgdrhylCry"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(129 113 183 200 13 76 144 187 88 174 84 57 57 33 171 156 92 11 49 150 240 69 233 254 92 139 22 143 14 95 106 119 225 170 52 236 237 197 72 28 229 90 179 76 20 224 242 232)
      #t
      ())
    #(245
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "O17tgHJ7y8URO4qeTbHIGx3dwtmf9W2cPBBUNIkTveKWMRxL0vqJm00OZqqhtqDd")
        ("y"
         .
         "e38PKNVeLzpQ8fG-85doNKBbQ0GOl5MDvANj7RbS0LQBHMN7PAatcxVPrqt5Fc2H"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(31 230 254 165 240 13 48 5 171 170 226 38 127 241 142 67 9 21 131 141 135 144 154 181 3 136 94 223 56 190 118 24 236 179 33 240 164 223 113 176 145 63 191 18 199 111 193 240)
      #t
      ())
    #(246
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "apmlrNSn7bHHB9f4vhLoEUAzjj4UulY8cDxoGjGaP5zh-Q8DK_hA83WOicuFLOym")
        ("y"
         .
         "PPme8E9RpepjC6P5-WDdWToUyb45_SvSFdO0sIqq-Gu_kn8sRuUqsG-3QriFDlIe"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(245 138 220 19 255 153 125 56 56 57 16 219 123 239 177 118 112 57 58 51 217 91 4 156 42 161 157 118 12 142 114 142 206 221 50 22 132 118 185 11 38 163 116 45 204 18 27 7)
      #t
      ())
    #(247
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "fCWk9X92qxOyXKs8Jl252b2SX-y_e_k77xMId4ZGYo3sqwZ-2YipdVzYjojeNnEE")
        ("y"
         .
         "Vi7gxX5x2Wzv4xtMQEW9QIajjoq5rfLVVnvjGAUdcPOqaLdT8nGrAytqvM6Rnili"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(86 41 150 132 236 92 235 9 186 77 148 209 35 16 5 168 38 201 192 138 82 25 199 87 224 19 108 190 139 100 48 186 221 73 37 23 47 41 57 137 29 167 199 137 56 80 81 47)
      #t
      ())
    #(248
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "f___4AAAD____AAAAf___4AAAD____AAAAf___4AAAD____AAAAf___4AAAEAAAC")
        ("y"
         .
         "RICrM8tL98t5wCTureP9ZB4vMANphADomGpzQ6XaWaOybupLQXblMjk3FDfYNKGn"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(25 17 160 238 106 235 226 99 253 207 61 176 115 242 89 140 218 250 190 194 18 58 47 36 162 140 61 145 81 200 113 243 45 109 194 243 29 37 175 156 73 143 214 141 162 62 91 239)
      #t
      ())
    #(249
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAC")
        ("y"
         .
         "B5faTAdRztFt6A0Wq3xlSl3CfQkmJtCGWhkqHF6nwbiMn8qwV5RnQeQcwoyA7Aua"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(21 144 6 67 226 224 88 57 118 151 75 5 248 60 122 150 97 20 37 247 196 166 235 81 145 106 185 88 160 55 253 156 193 114 189 207 255 69 64 162 255 60 230 78 101 5 85 126)
      #t
      ())
    #(250
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__AAAAAf____wAAAAH____8AAAAB_____AAAAAf____wAAAAH____8AAAAB_____")
        ("y"
         .
         "bHCJiub7MfovCGViry0QSGukxv1eQd_kqmFZi0cHo7wnamL-sbmFV-OxfAJfet9O"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(136 165 68 167 105 213 195 74 5 20 22 189 80 157 250 201 17 134 63 96 76 131 234 132 75 240 228 197 194 114 222 200 109 5 122 136 177 82 169 39 71 1 147 140 112 89 0 195)
      #t
      ())
    #(251
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "___wAAAD____AAAAP___8AAAA____wAAAD____AAAAP___8AAAA____wAAAEAAAA")
        ("y"
         .
         "DrFZKFi25uOhmcDz58XwtKkpFZNu-4vAQHaA63J0vnQiFWzoz8i1BbLZAsOZkjgP"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(183 219 38 178 134 231 82 124 177 244 84 120 47 229 65 134 47 240 248 215 238 217 96 226 40 85 222 183 172 42 105 97 22 104 199 119 197 59 183 76 43 205 64 237 251 247 148 77)
      #t
      ())
    #(252
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "____AAAAA_____AAAAA_____AAAAA_____AAAAA_____AAAAA_____AAAAA_____")
        ("y"
         .
         "SYerrkEoCcL6SP0jsb355iL1pgbEQRchX_phsY70blSn-78R-aa6WcmRtK5QH-3O"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(177 232 170 177 170 99 61 152 220 107 118 133 148 225 227 237 184 1 169 239 72 63 40 124 131 225 151 68 210 173 52 58 211 222 189 196 220 23 130 19 173 104 118 181 34 132 245 82)
      #t
      ())
    #(253
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "________AAAAAAAAAP________8AAAAAAAAA_________wAAAAAAAAEAAAAAAAAB")
        ("y"
         .
         "NpH-ST1NKL-O4d_sgS1sMG6uCEKRntptxSXw1JrC0mqZIlGRITmik2hJ-db6lJpo"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(176 222 0 111 128 246 248 158 78 234 110 70 223 227 5 21 48 5 97 45 30 144 49 113 236 40 134 35 9 113 150 27 82 2 169 243 24 123 218 196 19 172 36 200 54 173 247 160)
      #t
      ())
    #(254
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____________________wAAAAAAAAAAAAAAAAAAAAD_____________________")
        ("y"
         .
         "YVhCqgawb3jwpm976ojUtu5ZZT7qoA3F4KK2WPlptxr5DJtOlr08ozhGlVvcy9NZ"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(202 140 250 66 197 227 116 145 76 20 214 64 43 26 153 32 142 71 224 46 196 152 24 145 54 148 234 8 34 162 204 108 49 2 89 168 243 171 117 89 185 151 75 196 194 250 51 126)
      #t
      ())
    #(255
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__________________________________________7_____AAAAAAAAAAD____-")
        ("y"
         .
         "cyFSRC-27lw-bOHZIMBZvGI1Y4FNeQQrkDzmDx1Eh_zNRQqG2gPz5u1SXQIBe_2z"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(237 240 64 186 206 24 217 11 249 206 114 13 242 163 179 29 118 217 91 126 217 83 10 21 154 192 178 78 130 168 113 3 62 173 164 5 82 249 230 6 247 17 94 106 120 146 117 17)
      #t
      ())
    #(256
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB8DEjsAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "cb0ecAw0B1w8rejOKdM3JK9op2crJlpOFXBVNgRAq3xGG46ayAJOY6i5wXwAAAAA"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(234 129 125 255 68 241 148 74 56 68 68 152 241 182 193 167 10 139 145 58 163 38 188 42 204 80 104 128 93 141 221 122 94 65 184 238 91 131 113 161 207 63 122 9 66 88 227 166)
      #t
      ())
    #(257
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB8DEjsAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "jkLhj_PL-KPDUhcx1izI21CXWJjU2aWx6o-qyfu_VIK55HFkN_2xnFdGPoT_____"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(234 129 125 255 68 241 148 74 56 68 68 152 241 182 193 167 10 139 145 58 163 38 188 42 204 80 104 128 93 141 221 122 94 65 184 238 91 131 113 161 207 63 122 9 66 88 227 166)
      #t
      ())
    #(258
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADaikHwAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAH9XtpoBR4Pb-klnsvnPpnim8Lbpz9QWSM7Fs8SY5yFS2j-C09oujp-O83sR"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(191 169 62 24 79 118 39 159 215 7 213 61 220 179 98 136 85 207 175 177 17 188 189 11 77 246 239 119 174 230 36 146 77 104 22 38 161 83 250 78 89 201 35 183 31 192 144 179)
      #t
      ())
    #(259
      "edge cases for ephemeral key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADaikHwAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "_____4CoSWX-uHwkBbaYTQYwWYdZD0kWMCvptzE6TDpnGN6sJcB9LCXRcWFxDITu"))
      #(("crv" . "P-384")
        ("d"
         .
         "K8Fc85geq2ECw5-aklqhMJ21nCwCpUQRko1zw5RdFXhI3DaVnv73SVyFKOooTByX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y1Kls3Y2tCEAc0JSdEIrCtba2jk3yUYSpIyyiS37wGwyrdu-neoULwnDul5Y8VTu")
        ("y"
         .
         "IIqE4Mc6BiCHtJwtE2syzqtJrZ3c-ukkQCnEEgy7n_YIfy6-VojCB5emR8hx0NEE"))
      #vu8(191 169 62 24 79 118 39 159 215 7 213 61 220 179 98 136 85 207 175 177 17 188 189 11 77 246 239 119 174 230 36 146 77 104 22 38 161 83 250 78 89 201 35 183 31 192 144 179)
      #t
      ())
    #(260
      "edge case for Jacobian and projective coordinates"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__________________________________________7_____AAAAAAAAAAD____-")
        ("y"
         .
         "cyFSRC-27lw-bOHZIMBZvGI1Y4FNeQQrkDzmDx1Eh_zNRQqG2gPz5u1SXQIBe_2z"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(195 211 132 80 165 223 219 230 79 75 93 163 173 212 172 19 21 248 125 17 114 15 41 190 182 243 97 106 101 235 202 29 86 156 15 174 92 91 187 109 20 107 249 16 62 100 91 159)
      #t
      ())
    #(261
      "edge case for Jacobian and projective coordinates"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "GDwNNlDTWjNCPA0AiWqgr580Ahf0IIa6USA9FYpg-pHug7UgNO5hZ8ZZPOZ6McOK")
        ("y"
         .
         "IIE9nn7qcY1GSFZQQgNm3MHP21zek-qygwje91i6TDujhhOdSjo72tZMVBkvK94y"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(24 21 9 97 133 158 62 250 214 38 117 130 249 116 23 241 158 216 231 229 55 182 146 185 16 80 148 74 111 171 128 48 65 29 217 46 148 19 87 199 73 74 240 17 136 64 166 20)
      #t
      ())
    #(262
      "edge case for Jacobian and projective coordinates"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "zlrYEefrCAJOQOMMHLKcrN_I-An38BEOayJcbYM29gwwpXPvG5EvX9MAsGFen98f")
        ("y"
         .
         "YxupdL65VEtKjJlF9y8-X8VpQ4xDA2ciefr6kA5TaQr_dT2pL0AKti3oBn2pn-N7"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(205 252 148 99 175 137 22 130 197 27 111 76 150 86 85 28 51 184 7 57 241 119 97 47 39 164 177 243 124 151 253 128 35 73 91 11 193 92 104 138 177 155 127 152 128 239 200 197)
      #t
      ())
    #(263
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "QvqsKuK7rUOm5Fp5bx3McMrEROwVcKx2nW6GIHRD24wDozugeGL8xhnOyAbOiCbC")
        ("y"
         .
         "d24x3QhsGxkyO1gIRVO7CCXQkf-xI9Jlr6WXDLcsLmgEqzJ4NCmWQecP8UKRl1ux"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(190 228 108 85 24 232 226 245 199 250 35 242 171 143 196 63 90 116 25 247 3 115 218 161 166 116 99 11 22 30 163 156 56 26 30 170 169 14 253 52 174 250 154 198 237 181 60 117)
      #t
      ())
    #(264
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "atZvZz2DL4qGz9embSFtI6JlgyNcKOy3tZj_jdETK8bPDcRqmMhkZl8ZWpllY-V3")
        ("y"
         .
         "cKqq3L82UxoxjalRINmoACJqyfv_BvR4N5Ece5X1GHxD75WaTqPhF7hH8d0fjMSy"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(30 242 228 133 83 30 38 214 154 183 199 184 194 124 27 75 225 82 114 137 97 135 86 129 4 38 131 103 140 123 180 239 139 89 199 193 214 42 219 95 3 75 178 136 111 246 145 233)
      #t
      ())
    #(265
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "-ELo9HFRlJtcts3krMeoPZARc64xbsQj4clmyKOWOP-m3vMWD3vqZkmCuWNvY59y")
        ("y"
         .
         "cC_h1ZNFT3PQYFBlXijsuM_f4T3Md1K944_gBwtDvaXmtyA7RQd0cYJd8bgWpCjJ"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(168 87 59 73 0 99 98 217 183 54 60 80 41 105 178 35 59 47 197 115 12 245 117 212 20 197 10 69 240 165 180 213 24 189 69 2 136 200 158 129 213 23 30 250 219 206 89 197)
      #t
      ())
    #(266
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ZOaNio5tRBQv0LM1MnXfS-lbD_lGXRFKGP8jj6TTlH_04_xmlw2Fcx14SwoXFXiH")
        ("y"
         .
         "y5Yc9jbA6NS1-NsAYMK2me11DpKgWsTaSXgLikhynGwrhmx1dUr7LHwtZh7PZAzC"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(51 249 170 48 132 69 205 194 209 89 205 57 195 59 73 83 130 255 139 158 33 182 209 177 96 87 122 75 60 93 175 0 184 218 211 200 56 82 241 143 39 164 64 44 233 68 33 238)
      #t
      ())
    #(267
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "I35aBaO53RbyfRzATRbxnsPSeIG5DCBJ_SZlzsfqrhys3tTg-Ku6WmTUGDpxRJ2b")
        ("y"
         .
         "dBBz3CWU0CrJqImI5R177iIRGWL2N0jv3IwxH2Ma6GgMiZYCjaTpVqwShNDAFhT1"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(221 247 254 165 43 146 225 84 79 177 199 206 34 116 64 155 202 23 4 244 75 192 174 26 106 139 56 142 175 49 108 144 188 233 155 12 32 63 117 86 229 186 170 38 198 183 173 109)
      #t
      ())
    #(268
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gQ-s6ZUSvsJZ1XUinZWGeRvfsbIh0jwXOOT5rwSY9UsYzAy7AbZM80EexLaEgNtt")
        ("y"
         .
         "bAbmIJGI81QyO0229U8rHfxcHD3U_vDLN5p9fqCFrNrJ5g4E-NtIi-l_gd7WbqM-"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(210 40 124 203 100 42 118 224 1 18 77 186 108 108 225 158 130 83 102 144 171 245 66 16 76 189 69 200 112 142 70 45 140 224 110 5 173 42 15 234 77 104 73 47 207 55 106 254)
      #t
      ())
    #(269
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "8LACcaK93kvdvawAQGrfBW0yubA_S9KcxmCT3yLl2gn-SMq_yNRL8Ta1vidcf1P2")
        ("y"
         .
         "bM3Q0ANUowf-oAkQcLYfqeLaHi8yn61oATRJ7PR3_jNFgDm6yREQTaYsKcCBkqMV"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(54 21 27 13 28 27 40 172 6 46 8 41 244 27 213 80 68 218 230 241 218 243 10 179 255 65 72 168 60 203 204 245 7 190 247 52 12 149 68 240 71 245 84 199 18 114 174 36)
      #t
      ())
    #(270
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "-Cy7rxybYGOhtQVYB2axRGtrWpkBUXA-ev2KMXU4f65WcIEBng_bp5ZO1sDk0ORe")
        ("y"
         .
         "zzebwe2jDOT6VgEGs9Cb846fy-w-rcnhLFYiwBEwyHDGE2SY9Sv2LzmH3v0pTfM8"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(36 81 253 151 152 100 22 148 98 167 125 116 42 219 111 69 5 242 219 149 109 45 45 234 91 5 228 15 64 242 101 79 193 125 28 209 211 179 83 50 43 240 209 243 207 130 140 73)
      #t
      ())
    #(271
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "zOwRcCXuzing7Qd9mGeHOP0aXMog5AuzIqQFZoMSQ9rIBlVbhQj06qaL60547ZF2")
        ("y"
         .
         "hkFfESMsXsufgFsNzGzga1f99OSuTm2CJHLWclqcHaQcL-4VGVhsUCu7GkziWChj"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(11 60 6 251 140 124 49 169 111 166 220 128 37 1 67 227 154 249 229 194 1 253 190 146 36 87 113 227 69 207 120 127 153 35 78 204 29 104 112 89 154 213 10 28 173 246 135 233)
      #t
      ())
    #(272
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "HoKb29iiwTIdBJMqrNQpcvWyrFtqYbBBIWypSBuw2oq_bVwbY-hoR5-I2RNFy5FT")
        ("y"
         .
         "_KLFaY_VtATJvBJb3Dxgj-OFNn7itoQI6Vm4Psi8PsHgWm-5hP7bJWV0iF9d2YlA"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(83 136 144 90 238 127 130 107 154 91 70 250 239 254 38 212 159 133 102 199 168 232 175 90 22 83 252 251 255 124 13 253 229 102 240 67 64 18 72 72 111 249 28 57 15 92 198 237)
      #t
      ())
    #(273
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "TdHhtFSBTzvhxi64fBIHcsIGKj1-VGHVVUs5Y6n7EZDbllaVyvVVpFriW-c-R8N0")
        ("y"
         .
         "W6qT_iv7CU_nGMjJvY4ebobNYp3KC7ke6TTpNGnU__MqQd_XjCe6U0o9WXDDiO_7"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(185 32 41 155 162 96 117 210 100 124 203 151 141 21 165 78 244 131 48 219 58 135 148 216 104 174 2 191 21 43 60 36 255 169 102 137 62 169 235 93 60 206 131 224 35 155 224 44)
      #t
      ())
    #(274
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lHGr9P0bidFN8jJcsKa8kuc8jsjgbEewl42T6SJa3widPuTiXCIrYNAhmcG1gATB")
        ("y"
         .
         "0bNbwUdc_PPjphFlk-LKWnA0QSetkHpKodDGYTnyh6f-SM6lhEog31poi4khHxnD"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(112 84 33 179 102 24 17 95 204 183 206 109 184 87 95 196 179 240 162 120 12 224 42 52 135 228 5 93 208 213 24 52 0 98 112 233 146 224 175 170 76 124 224 246 153 247 254 162)
      #t
      ())
    #(275
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lwRsRDiwQDFb457jhp8-hxHDGWdYR28rTZDNQ5lmXIxch_nPZCicK7ARZnQuGuA2")
        ("y"
         .
         "soGSLohytIt9PhOEq00KksYCrOffOpOkfcUMjC5eioddLZJ_cgHOEf2N-BK6oeT4"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(60 81 0 6 248 101 143 63 164 20 254 249 114 61 99 74 23 157 183 170 240 183 6 139 118 150 207 249 63 136 224 65 54 223 218 198 131 73 120 27 240 90 120 122 225 158 144 33)
      #t
      ())
    #(276
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "jj2uUA_ixkWqsVN5FwaCUDFUrx_Qoh_9LjTwUBxRhQfg6_RdfLhlP9V78fiU8qVS")
        ("y"
         .
         "y2mvfgCdQ71uaIvyj14oFfPJ9Pqx2CnSnwUxK62Z11wlGNKjAadNXs7hMzWC15wU"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(36 137 103 175 163 99 134 207 234 162 33 72 71 199 200 195 42 137 26 36 56 184 54 101 249 158 95 231 171 109 216 99 83 255 122 139 21 212 190 78 18 119 199 33 214 156 187 167)
      #t
      ())
    #(277
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "H8qPlpVlCgLW-4BJsVMaN4PkSd91qY1FmZ-5hXw-dWIZ1IsshNUUPwjCqwl7Nloz")
        ("y"
         .
         "RaoZfhDGWdnFP6dU8RLXeXMQfuNcOu47CxMis2L6CEl8CXwYgZNONs69XhjpJAy2"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(164 84 255 77 148 69 135 161 136 231 235 172 205 151 100 154 176 207 131 248 248 209 155 54 83 162 230 33 32 109 57 146 134 139 90 87 101 209 250 118 47 14 59 1 97 166 128 155)
      #t
      ())
    #(278
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "cxunhOLtIfSkorTdSHeuwvzVkDH4AZO5VYu8HPf-ZwRGlEP8fzmOi-xAhviKp2qi")
        ("y"
         .
         "bDiaz9UfaciaFTv6211wOjbLEqTYX1UXz0Yr9dAqv30Zci9IDq8aJhfLoZQeHHa2"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(153 120 14 210 21 220 65 127 121 140 36 50 124 60 175 34 243 104 94 92 242 171 131 95 197 75 66 129 23 183 209 127 15 235 99 241 117 84 104 21 127 50 55 184 17 125 255 166)
      #t
      ())
    #(279
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "hyXLdUtVJ6OwU9Y-DEsJfkw0xdcxq1zUcoZQphi03tS_khTXk610z8Rw41xUk6S0")
        ("y"
         .
         "IKPFAEmkJ5FyG8UNX2CMsnK2ClVF2wYIhVusj7-pOZDWuA4wQXc-EJN9g3AQyuZA"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(215 56 183 55 63 233 120 242 123 25 79 105 237 213 196 33 185 137 34 245 227 19 183 104 89 169 53 122 31 175 66 237 14 6 220 19 213 88 67 53 117 56 206 127 65 39 206 225)
      #t
      ())
    #(280
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "nHpZTq8t3zyKtsbpTx7AEdrJ-kK9J98vvkAqyUnTosZ2toSiTJn30bfonp-KBjTr")
        ("y"
         .
         "O4MJXHoc1qqTMkYOy_JX96-kSWCLng3dFSkrh1rS_QadySfOAKS15nkzgCO9Ok5s"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(217 204 134 29 179 5 176 74 39 61 107 85 52 142 249 240 160 162 174 223 139 61 4 26 25 226 99 237 106 226 224 30 67 154 70 130 48 152 242 170 164 134 125 79 8 137 30 54)
      #t
      ())
    #(281
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "IbGqNZ1y_zOoTsliFIPmWIHU-ozm3zZp4Ku7n2v40O49mGKn37RSE-Of-sez4e2H")
        ("y"
         .
         "H15ZzaWluGGrPrhsqnxuhZM2du_mYN_4rFYl7Ip2MKnLqes_wUnT7zedSl7BdNI3"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(197 146 129 208 159 68 246 121 41 74 238 86 104 235 83 141 80 62 242 90 172 89 22 20 180 111 82 93 140 11 16 154 177 41 103 73 99 80 10 215 182 38 137 65 162 140 0 103)
      #t
      ())
    #(282
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "4a0raPppRWV03pWx4EiwcVAJQJ71zCcL5R_dQLT2WxreZ5OKam1yWwExDyJTo4z0")
        ("y"
         .
         "RPcms-tcXdwlfW2b2pv6YL7GoH_gfa6WdRExQ3DTpOBBw08ctJ_mtwZj8CBIjpio"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(158 183 15 137 73 134 28 48 242 69 105 144 171 228 211 69 85 0 8 184 83 25 1 82 19 97 170 161 114 150 219 202 6 251 60 135 232 244 89 43 125 197 209 153 124 88 192 118)
      #t
      ())
    #(283
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "C61n6gzKlg7T6gWI1e7vNmwkNCqz5GYCiR0a202F2A3SQpYsVlS9avWROurvqyet")
        ("y"
         .
         "mge0GXGAyYaa95X2lv328lzcZkafTKfKicXJBGNthfa5oZ57zwVAgxynqkBEbdjh"))
      #(("crv" . "P-384")
        ("d"
         .
         "k489vjcTXNjIwEGClS9ub5v-ekz_Tqz5BhKkiGXcQ-m10jD3YdH8TftpviDoTMyP")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipxSJ9E5Alm2dQlZr275vApRsPCFyZkz9Wp2OmC1kPcRXwxs8eMNNV6752Aey00a")
        ("y"
         .
         "u9LfZa45mnF4DeEePPjxqjKVL8SxSIKERS9DqQud0fMaOqynGRFP5wU3-cbewHlR"))
      #vu8(69 176 47 212 221 118 73 103 1 190 59 55 230 50 5 108 247 246 62 152 8 246 39 250 133 142 236 197 186 193 106 1 169 73 116 27 77 225 81 254 53 231 116 198 209 227 137 226)
      #t
      ())
    #(284
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "LEBpvIB4I26yOoxNV1vem1N7E7eZiWiMQNidiYqMKb16xhdFeXt-5VrKYOp9bCqE")
        ("y"
         .
         "MPz77RiiUEZh4CU8Q-R74BlA6B_LLWWl5QERGXgFmUnF0lsLYN54IUdriKg0Kgvf"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(108 233 191 251 137 1 186 168 1 209 177 158 146 181 229 244 127 91 238 16 193 95 228 75 130 9 151 117 114 204 139 215 144 44 207 192 215 174 152 246 181 139 184 127 49 106 87 146)
      #t
      ())
    #(285
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "RLwcstQv-XTnK8iD3_eE6Gl4UB-Rju9NltblJC2m9WUaKc9SBcPcO-MzMvot78NH")
        ("y"
         .
         "iPiPVJ7XhzIGMCDdP8uHJRWyXgiHAOPqy5Jd52fp7fbonwicT54PTYhGmAOTZn9t"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(198 221 45 55 45 90 81 115 172 99 246 213 171 93 236 151 46 138 228 77 86 108 127 130 107 235 127 38 147 226 0 196 112 221 237 177 201 146 156 17 237 134 209 53 244 76 151 159)
      #t
      ())
    #(286
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "uCTrcsQ-21s86a-U670ztEa0pyM7ia_-HPo761PNGZ-xGaNWdyv-wXjAOwd98S9T")
        ("y"
         .
         "-ZhR--uZcf_PKa1hukhL0FgANtyqWfQHvIikcc2GK4ChY8kab2RgK29xOVmy9sL9"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(2 153 183 252 62 85 6 146 111 234 102 210 15 101 185 121 31 182 200 140 113 121 247 212 112 29 104 193 52 39 171 86 172 15 158 29 169 108 29 95 61 26 101 255 49 34 193 83)
      #t
      ())
    #(287
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "knC4zvthpdUvu15rO2I2Hzr6XRprAUGduSZuLbq6s2rqWxlhMY6vjwvXzKdf8t9b")
        ("y"
         .
         "flOMYwEqc_d3sl4XN9RP28KZp2KI17y1fISExxybtkUyRfI5tsbfMomEJDg7TAMJ"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(19 215 139 122 77 175 141 221 98 227 71 186 35 92 54 72 111 178 235 74 117 163 225 122 19 126 198 194 222 55 171 175 69 218 146 209 207 27 41 102 222 216 213 56 31 210 49 159)
      #t
      ())
    #(288
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "GeXChldHnrQdNxj2PgZ4vg2qWFAqVAbWWRNWffe4arJekF7IRniVMNgdjR_CqMre")
        ("y"
         .
         "Sq8TGgzPx5okvIiclTJJ7oW0niob-lIWf-IN4pbmSX75AUws0KAq-60fnQCsPZJN"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(123 8 99 252 109 114 196 189 5 133 118 207 247 164 174 223 134 72 45 61 155 201 197 252 85 189 143 172 132 16 125 208 191 159 39 215 232 214 36 237 176 170 110 241 185 204 34 70)
      #t
      ())
    #(289
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WWZt9EfavDhtjd2uk1Q9x2FR98HASFhmDZyo4GVbUacv7_twhbrL3hU_6nW1cWq1")
        ("y"
         .
         "w8eBHQx0Nb6eNtsAzSnGyoee-ZGtsVemtL9W495cs0WYt5poTMPU4j8i0QBj6NGc"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(251 162 189 251 84 228 104 164 255 90 251 170 84 199 3 116 0 215 100 20 201 142 175 170 65 154 218 176 223 80 201 105 42 172 103 212 74 158 251 208 13 79 156 252 32 109 83 123)
      #t
      ())
    #(290
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "O6_OmzJhbzveF_yYnx5Vb5L69cMOLN3rwqukTt82rjlXycYQo0Uh5qxFRlKqWXgR")
        ("y"
         .
         "gntP9F-ZH5QF8tjPRdXQh2omDvDetuhgkMW0X1t7F4YQUyT0Cg0rQ97VeGizi0Pv"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(186 155 104 209 192 10 161 36 110 218 251 112 219 109 218 237 133 112 196 101 217 165 131 109 65 86 161 172 127 218 42 32 156 37 71 26 13 246 215 222 147 249 128 200 156 132 22 32)
      #t
      ())
    #(291
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "UzhbkwPSxdHqg-dwCz6BGnyQXRd1w3mkTp5cloIVWXALSS7ihoxaAZUKpOKwWTLu")
        ("y"
         .
         "j6G9gwNaq7q8K-e2HziPeLgEbTa-6ZtUibr7Oobe3inBTVNz7ZuscqkquOfDH7hq"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(51 198 199 226 200 5 136 156 170 229 103 154 160 36 221 121 245 68 86 247 152 239 240 160 221 204 27 40 216 173 48 45 47 103 9 34 117 255 85 248 244 217 147 87 172 134 206 88)
      #t
      ())
    #(292
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "nr-NMT2j5bojfuM-bdW-dwOQ-DfJUo-91ECp6yuhz_sng32GXL4bMv-ceZXV3VEc")
        ("y"
         .
         "qkEl7CWNGC7n9xc8odFxOa4T5gcqzZcLpW9tUDTp9F2jay7HdDSb4haPMrvwT5ox"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(13 238 176 29 236 170 224 238 229 94 44 41 80 28 221 156 247 212 219 47 22 121 251 114 239 202 165 219 110 5 204 74 101 132 128 36 92 89 254 201 152 76 97 53 1 65 27 10)
      #t
      ())
    #(293
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "1M0i6N-mIMp9aOKQRRsJKFKYodtafKAOqDDW6eycxNA_XNQ_sqmqqhQv5MbiJpzc")
        ("y"
         .
         "dH4MoH449MW0EuUst5MMOBEA0qr-VLYZ6_cugaRuEUljXSWjf6ZuLRvJrCX8GoPz"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(211 96 117 60 86 134 152 215 247 178 97 221 47 80 100 255 200 170 159 3 52 58 34 145 185 103 99 38 70 213 21 112 86 47 26 232 119 117 100 142 116 249 243 14 150 110 115 128)
      #t
      ())
    #(294
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "IbkbnDdOz1ztxtc0IsZTmyKuXbYb_bBrnInmgRL2Qz6dSXFfR0U0-5zWwsv83vlP")
        ("y"
         .
         "tdT-MG1ZAlr06I-Ms93D1k9wfmggtBzfwFXNZ2LaI94BwbMR8aW_cBDE_07Qp-hM"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(89 106 247 18 9 205 210 60 56 107 184 97 237 189 23 30 169 188 106 133 137 147 17 12 147 51 169 86 179 160 63 192 245 83 147 202 70 187 21 188 249 241 43 73 100 89 196 230)
      #t
      ())
    #(295
      "point with coordinate x = 0"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "PPme8E9RpepjC6P5-WDdWToUyb45_SvSFdO0sIqq-Gu_kn8sRuUqsG-3QriFDlIe"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(150 135 110 158 216 158 223 62 61 253 142 173 6 220 186 10 205 206 119 99 235 152 173 237 130 151 111 163 97 129 199 154 196 232 51 223 56 90 245 238 134 12 161 242 13 244 203 114)
      #t
      ())
    #(296
      "point with coordinate x = 0"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "3WB-lUVWxi45tDLR8bL-ZlLXWh_jPNFceZ_6ydE9XVIYmdJZQNplPQgkrYRW3fti")
        ("y"
         .
         "n0FC4Io70dh79hGanvSwqA3HEqTWwAG8snlMTRoDsfHTktwIjInvhE-TrbE52Va4"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(96 213 151 169 156 134 164 50 225 68 188 43 14 10 137 185 28 74 114 243 225 255 142 86 231 240 125 83 178 229 99 233 121 192 158 39 179 75 233 139 177 152 101 232 8 212 41 14)
      #t
      ())
    #(297
      "point with coordinate x = 0"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gm1xrk97Psuyqb31lRKUlULQEW9hZEClUBaPS1iBL9bBq5TzEDhf3yRtte_A_ROZ")
        ("y"
         .
         "XeCZvfzqNV7mOCfPKi4tho9c8LSH5BJF31T9xzXu1utybPySi9W00BcZ0WRJZ-0f"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(133 150 25 6 237 170 154 223 169 201 184 39 157 166 97 88 245 127 179 80 255 155 21 220 77 117 184 202 182 75 157 52 245 206 145 47 26 165 18 165 76 113 130 28 110 49 255 143)
      #t
      ())
    #(298
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "-okZ5FJOcEBInMF6o9Yg-IJ27ySNHs0emmY3x6BnZfbx1ZOx_r4zG59pboLFvmGk")
        ("y"
         .
         "oHSpEjCWYvRM4IJb8TRYIRbKwzEFd9WgByK6UT5jNMwXEBCRIjNG22cGPPbnv_v_"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(67 72 77 207 1 73 46 171 104 77 57 136 25 8 218 127 64 17 159 231 24 59 186 98 92 50 253 193 77 163 196 35 109 195 57 114 52 231 219 65 133 116 149 205 13 9 73 27)
      #t
      ())
    #(299
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "rsuPFauHV5oD9bj88zqglDN-TDYjWcnycnY0kJxpE8Mhvmnz-K7Wk6MQWZiZl_sy")
        ("y"
         .
         "1PSjf_I22PYjcYajpyGoKiuiL4pijXJz7zFz4h7MaZaaR-ZpOD7PDPU5brAvRsPt"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(101 230 19 57 41 200 176 97 194 95 224 190 201 20 44 147 213 44 159 22 149 6 27 16 95 108 125 161 52 122 150 123 90 63 17 131 86 86 69 247 184 250 216 134 63 154 99 69)
      #t
      ())
    #(300
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "QNxm2BxOFeGkwAnGDDRBcl__CLJUN3Ow3eANsjH0S63XeovJYZR5vVKI5A2hU7jd")
        ("y"
         .
         "w9UwJisWmm-nAO4ypc0Fe69D0pYn80L9qPbw-yGx-jW-lqRSq0iB9BfJpOQTax5t"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(7 115 103 61 174 51 122 22 136 194 195 29 26 96 12 14 215 38 4 161 113 198 101 181 48 126 186 247 64 253 81 142 74 124 92 145 228 248 111 101 144 86 64 238 55 244 248 48)
      #t
      ())
    #(301
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "U7WHRzgnWi4dc05R-DY2-JF5wW_on0b02OcfUhxZR2nTFyX2QEEi8hESlDoaIv8H")
        ("y"
         .
         "WGtBuyS_MDroVFsl97mEJepixNO_UdI9nYXgdxm4KHd--goo-zDzO4mZgN48UyYR"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(234 37 176 122 22 31 49 138 129 166 147 246 96 72 16 184 20 40 67 196 253 38 101 141 155 100 254 219 88 165 235 165 140 33 124 65 118 86 156 243 63 164 122 201 242 179 215 245)
      #t
      ())
    #(302
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KYit32IUnmrAghnS8DbT9XIKqGKQXPP-e98TbcCLhF6KYb5D24bXGibcF79VN0qd")
        ("y"
         .
         "U6yZxdB94QznTtOxxBNobrunAPSSYLCDV_ldr1sxpVU0XAdgefJdhpD8GsIMcVKN"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(168 212 36 6 143 183 132 107 13 67 165 204 19 2 157 5 75 17 201 126 80 142 224 249 46 115 211 90 200 53 26 70 46 237 252 157 169 221 18 121 241 40 18 186 70 192 96 83)
      #t
      ())
    #(303
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SUtZ3htpU0tLYtdYEO49rQWhmvpBx4Nttcp4YwIIqAA4jQQDS68pI61a9qxy7gX5")
        ("y"
         .
         "id9tCPmmQAhY0IT-dwwN3eKYlmzcVqkhpDGEOE1aE_McHrToNOI77TBpwQ3-1rY8"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(41 111 93 111 228 221 216 195 199 48 75 234 221 69 246 205 62 63 205 66 48 201 210 186 26 19 189 155 148 152 163 200 209 239 10 182 238 42 69 11 12 104 232 154 131 113 31 162)
      #t
      ())
    #(304
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "tkxJ3fGLznnP87jLew5Y1IQh62c59Z0nH8T1TdL8sc9W3Ts8aRc6XqtQqxouBhbQ")
        ("y"
         .
         "XZK4ZMvJ5KP3VlDv854OrL-HxMAQCI8zOIQs2Xgyck95uxWYa9P6hSVYFjUEc7vT"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(219 30 219 187 31 58 198 223 159 70 18 136 18 188 231 129 139 93 116 92 117 50 42 154 190 137 152 59 232 104 170 26 172 85 95 77 96 236 39 39 48 187 74 217 73 165 61 56)
      #t
      ())
    #(305
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "pAscoaUajua-9bWKqrz_4uFfdKMIftsmIIhdfZM-T1DWJeqtk7h7jf9Y8FZXkrF0")
        ("y"
         .
         "q4lxf_6HzSn_vzzX2TixFzdHv8AxG3hyNoTXyaHCvjtZe_kjbu0JACjtohRsR35l"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(31 132 198 65 173 225 98 162 116 171 46 252 124 246 88 47 90 93 145 206 121 147 175 105 86 213 94 234 14 27 127 191 4 13 169 117 4 134 176 251 178 189 112 217 204 171 183 133)
      #t
      ())
    #(306
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "TnqCrXSht4UZlfbhoYFBu0WErCLoOdTZFgGLFQ_tnNDM5Ce9RGeYUidE05XpDuS2")
        ("y"
         .
         "NEXBpqTNRnwgzbKYhU8eJwr76gU7d4SXa1BNDhM5xchgB87DY4OZWKH6UelIPeVh"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(84 239 185 187 114 105 216 107 6 40 43 29 125 194 138 148 12 215 120 156 164 225 4 196 187 124 94 87 54 103 108 62 83 112 207 217 120 73 225 32 10 3 27 102 131 136 216 208)
      #t
      ())
    #(307
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "m1Rnj4WBzmZRCKvrDAnLeYW7XdhYu7cciOBTRN5bRbTmfzOrWNNZ3KLajfEUndG7")
        ("y"
         .
         "0Jun9jqNNYSLQHRu1f7HlE7UT9mpovw9E-vFwF_5TV9QDuFQctG0wTdf1uxPMhJc"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(30 150 200 41 204 138 121 215 63 166 67 75 193 89 220 67 44 42 206 59 73 173 46 116 56 14 123 2 254 181 59 51 147 68 106 3 49 185 116 242 191 33 154 233 93 43 35 231)
      #t
      ())
    #(308
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "hst6P_Ou2y85riLy6CjsCBfZS4Ma5lTdZq_zGk2Esz-rRv7Bq-Gp5rv6xcmVZf07")
        ("y"
         .
         "svctVl0v9cXCTzdUSN17tELf3LYeBz5IQCcHu8wmrUPUeIqfRHFfG45bYr9Jzspl"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(132 231 175 23 109 219 163 236 60 231 183 252 67 223 30 213 155 78 50 40 250 63 111 180 26 1 244 19 176 166 199 64 78 67 247 116 113 24 186 38 75 123 2 109 113 78 144 208)
      #t
      ())
    #(309
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "zPWluA3rpjGLhfV6DVY-lXZcf-MN9ayIg381Mp3VEHDimJb02W70C8zPNiRN-DVh")
        ("y"
         .
         "ToeF-AZfgcXpJw_VWbajgLSDh00pjQ04Lu4e22pZ2cFjKWq3oGgYaw0j8zc33Q7t"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(130 134 78 244 213 253 37 32 74 225 89 28 97 159 62 216 19 199 98 186 160 9 186 59 73 172 56 197 123 47 70 101 47 113 154 220 234 191 188 175 152 91 41 149 141 31 66 12)
      #t
      ())
    #(310
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "x_d60obGi4j_pk_v5KqrPe4fm643t9Bpu5K-qCCJ6DGnDdntAHum3OFPnG1edE7M")
        ("y"
         .
         "sxmtbsE0N2ywWaHIaYTqi9FU2CbtPbro2NjgaqDL4BJ938MmIbvB9zH52Q5oenz1"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(29 13 62 216 193 224 210 143 94 55 102 241 147 218 123 220 161 204 8 55 98 147 249 187 222 123 222 14 155 30 188 244 207 163 198 131 220 99 188 180 43 45 158 222 84 36 43 194)
      #t
      ())
    #(311
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "qsO00oD8cKn-F14YgqaerKJIKgyd7ZIAREAoyj2_UB_DYfdli__sMhrVd7SMl4aw")
        ("y"
         .
         "f1lr2GjVHnPw98JduVaxIXHPqxGr1KuwClmsQywLDwWPTMpE-1gbxYA-RyoFMF8z"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(191 25 84 139 177 215 51 63 70 207 244 217 210 168 29 187 72 202 130 165 47 174 194 178 12 59 39 189 227 146 166 107 142 68 120 12 32 152 167 181 37 49 87 176 20 180 14 81)
      #t
      ())
    #(312
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dH77WV0D3AXdUMFxVeEBjKFHFQVFkpBuZxouIgSsrlUrb100XT_Pm0FxiSr7PlWH")
        ("y"
         .
         "2OcJsIOotTY2Y0kviiqa9kw60Bxk2PpHZNYfQ-aQGiRFzpS1hvYJCuBoevAJD81H"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(50 213 105 49 15 97 175 71 91 108 109 7 85 153 98 90 51 129 176 165 182 160 184 194 124 113 101 208 14 47 212 220 139 44 248 227 179 62 40 173 64 5 82 83 132 34 127 180)
      #t
      ())
    #(313
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "qxR2S_JPgxOxxJxL1uNnIxJ1swPGAcZvfosxBcawKpC2Dzh0XXVc5N1AaB7RWhrJ")
        ("y"
         .
         "PhTPL6cIMAorJ58sN_vA2nF1ySwRFMV-8lFGf28955-8OMsePbcOTZUiX72K2WiJ"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(38 107 38 27 109 217 235 81 119 15 108 76 17 184 113 52 171 255 22 41 182 81 113 16 122 155 32 224 199 209 104 225 239 195 203 196 2 70 79 54 174 179 192 39 185 25 142 16)
      #t
      ())
    #(314
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Hh_pcw_NW4ewZfqQppU0A2vkV1hRNEP3nGQn28MfxAPolAQ_ucf59YwKNnCxwNgD")
        ("y"
         .
         "WduPakiXDMuZGM7wTzUqTZGiBFAhGbK378bGcaa6RTfoLBlubUJoXFpvVZ5OivcE"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(250 159 32 141 206 222 229 151 245 17 34 230 39 75 134 113 14 54 86 31 35 97 231 97 134 252 183 61 29 127 41 40 66 109 152 195 246 158 76 244 141 48 1 214 73 112 66 94)
      #t
      ())
    #(315
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "g_zqIIx5Iny9lzGahll_hcv8Ge9gxbp6kvxSHGheIIzsdzW-m-J8c30_nS-qHD9G")
        ("y"
         .
         "laMhhNWPyQAUAkNs4AtSuIhbsRUPYeZg7OuhB2jQ9B1eTnwh1P2Qms6vYadFXdDG"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(154 29 223 61 159 235 25 10 38 242 14 217 37 255 189 53 223 16 23 196 164 201 72 93 52 76 184 248 28 127 69 43 47 243 174 196 75 175 52 229 8 134 241 131 72 11 99 37)
      #t
      ())
    #(316
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "K_JpNskWS6EHJUTEEHV2MLl9ToU_23hE8YEVB9BKaH2VH6ZaNDZOiUz4sIFduV9e")
        ("y"
         .
         "EGZGJ79vcjMKLW8hI2GiMxVau3naVDQ7hXLjDqzoyPVIAgDnUcIQIHhSiBLIPzz0"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(71 228 96 142 155 204 246 163 41 110 65 254 52 235 113 129 161 181 236 48 106 147 170 189 95 179 142 117 227 215 94 142 46 228 193 196 74 125 8 102 6 129 185 250 102 100 41 16)
      #t
      ())
    #(317
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "u1g9pd5MNS6KT60x6fcSl7yWMTTsGC6WN5xrKKEdJfpLd6Sg-UcIXbM7Dqd7naCb")
        ("y"
         .
         "4aC9EwA4Pr1gtapu6yDwPVbdFN9GFXRlFGJ4U268KMVHGpTwlVVMTAhaydzrHaVf"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(131 147 162 104 155 55 106 34 143 50 51 172 97 87 139 225 170 45 225 219 251 178 139 3 173 36 241 26 217 215 127 133 67 224 151 122 234 154 118 29 132 225 21 182 21 7 174 89)
      #t
      ())
    #(318
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "43eRWVtU6bF8wwcm0kJe6syWcvX7yR_BCM-MhP_5tm6mVIOEG3YYaUgV4aQVGGun")
        ("y"
         .
         "J6865sZY64xIXWrY5JweredRRXVi1gA951zR8eE-wV6h0EE2Au1_amt7hO_YowVu"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(32 148 23 218 114 27 73 214 77 250 9 152 58 134 131 82 14 94 48 10 82 178 232 241 103 119 114 238 60 8 107 51 154 29 162 92 171 43 230 228 77 228 82 108 49 244 235 188)
      #t
      ())
    #(319
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "fxLLmHsIwjVSNGUUa0IlhYQ5gGKhB0Kc5Ia0svJqligpOuTTCqYcZL-S-X5sCK6o")
        ("y"
         .
         "1mIc_OUnDNf6adywOO2LpCiv131SjCMPjlybg-9lnH5XP2ZxoY2LjEzrIPLXYIWO"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(231 234 196 96 234 218 110 185 153 136 204 57 246 18 154 116 217 81 13 46 195 150 1 173 174 118 178 184 253 97 34 64 123 121 65 31 142 122 223 188 79 13 197 13 169 242 184 43)
      #t
      ())
    #(320
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "3E8G1nHYYBLNXOEDd__vveZX2gj6klnkrv_mIRrFf24cW_hzKYnUTBNCqE-m_X-6")
        ("y"
         .
         "JxTOI8wPjgqL2lk-wrS1KU_8A2Pm6JQD1EgvNTfbqeTza-aNji7a_YzCwB9ffc7E"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(90 25 122 154 135 147 217 77 139 132 8 76 109 195 108 54 162 147 172 144 51 122 28 246 162 101 52 53 208 94 47 86 59 253 41 51 242 62 211 174 253 117 85 94 173 118 232 39)
      #t
      ())
    #(321
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "yh7GeYxWZ4E7_EXdGBXoK-yT6qcIEGl2e-w2hc36ViZ_wk2mwYbYsycWJ6hSVgxa")
        ("y"
         .
         "CX18QeYQ5SZ9q_Uldj9KSnR4xfmuS4C6oeYgrB8i2GibB-2amdzgzUoRaC8HIn_E"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(139 245 32 51 125 230 229 198 242 112 5 22 27 59 47 28 240 135 89 82 124 143 199 147 94 54 33 246 137 54 241 145 172 110 147 27 121 66 120 211 202 30 55 114 219 251 25 191)
      #t
      ())
    #(322
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "RLnpdrnomqMVGfwKNSiMVeLXYwoc0f_9KIzayMdLwu3LD_h4_Mzru3jfnn6wr6ax")
        ("y"
         .
         "OEeCuKETiUZZTY_Ttsm0lU5z2MBAp6FT89paQPn9rzvzGkUiQWwxbxDcfNivwoSb"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(30 86 220 48 218 211 62 52 39 53 53 104 134 5 187 152 173 124 109 213 29 146 103 201 10 98 86 45 8 228 125 214 132 24 13 74 183 171 182 204 20 174 76 155 214 174 158 146)
      #t
      ())
    #(323
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "z2YbccK4c0989RQqZu33h8VqMxdDA8IHQfB-DDSOG1QlcF2eu7RZlnYh3tbrVtcQ")
        ("y"
         .
         "NzN6oW4c9uUDGCGEyUUfm6RX2BoDE_uYc0O4s5NE82kymAEM1bc-OLzezmbGf6UI"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(20 227 169 126 158 230 144 137 65 11 114 69 152 112 73 90 3 125 64 119 187 160 39 239 244 196 153 86 125 126 127 26 242 114 14 31 237 208 123 215 128 80 154 69 22 250 92 72)
      #t
      ())
    #(324
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "UHkJCMBDNE4c9Csn5Nwzo11ooLk74Owr4FdFs8g8NfCj7MPtJ_sxiKXtnyTNvSnR")
        ("y"
         .
         "sPA_-F_X_K1OpbvYvUxCk18JV8qAEc_JPr63icRyFOYOXaFuTdh6U1R-7e5Hmwl4"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(133 168 221 185 153 126 37 37 205 85 87 33 242 213 243 177 153 255 118 181 126 145 238 198 252 125 44 149 112 58 26 50 244 110 61 70 149 42 144 181 16 84 197 253 4 107 49 7)
      #t
      ())
    #(325
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "I5XQB3m2x9Ta2MyaiRhJbCiy1Ad-aIbQob71FeMSXd2BPlqeoPGFzQCaaeqSIQF0")
        ("y"
         .
         "tnZ3X3Yxg8hvb6UFUboTfXmoPo4y_y_AmiwozfgM3a_MD0BpPjmF8o_lx9_i2LJ1"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(194 105 70 92 251 51 110 216 199 233 192 55 115 73 25 58 164 219 219 146 169 244 103 13 191 127 167 161 247 85 110 142 75 36 13 91 112 237 238 221 27 245 206 118 156 184 65 94)
      #t
      ())
    #(326
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "VjTuFVJjwH19HjRrho3dgOmigvzvk79t-Oi8TqNP8C4Rmp9weBbQY2jMuUxuSAL7")
        ("y"
         .
         "qtwzmRV03ehR5UfkcCf1Q1PgLXC-a_qfc5U8olkBHyOkqAy7quiAXZ0JTvFQ4MVK"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(106 33 242 68 71 37 34 56 159 8 78 46 74 233 155 116 123 57 203 238 153 108 233 163 250 5 53 50 25 182 12 32 250 56 52 211 155 11 130 111 16 150 183 164 103 232 162 24)
      #t
      ())
    #(327
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Y6psPCSaMuu8zOZf-hxWogVkSyxAuTGx7JsEK1LB6ZzQflOCmfuDf-Z1hqrnG0cG")
        ("y"
         .
         "FmrEUPvW1JIbRCoRZ593hrpadyqzPbAFQ6lBu8AV4jWBumJacsDk60SDjl9AbKHc"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(196 105 145 159 88 216 11 30 69 213 213 62 226 141 97 107 223 130 122 136 224 178 244 153 41 30 196 247 108 60 137 242 118 119 110 169 106 107 144 83 207 2 119 102 152 134 69 38)
      #t
      ())
    #(328
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ooe9dNUBwp3yPXhrfDTqDa8Q5LP85x1yD2GicvIhUdssWRSGdJMCjAHhA4ILC6ax")
        ("y"
         .
         "Icspj_iXfs8h_qjuG2un2bmNrMKkXXXGPsgyMAGpv19bpNPL_ugEgsoTM_TekKFK"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(24 221 29 123 107 188 99 17 175 229 56 61 121 106 95 236 19 132 75 174 37 112 135 37 85 77 224 181 240 62 192 247 210 230 59 100 219 95 131 212 238 152 144 198 30 68 44 65)
      #t
      ())
    #(329
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ukBrRsHZLUuj9At7H88eGCKJuEzbOH8o5a0g8e2MQ_5f-jcShGJ2MFHofw1ev82d")
        ("y"
         .
         "ndQQfOTZLXg9Dw-B16mYt2Bgc1tv1jv82XHKcBMv5tyfv4U07MdLUB6gHNLk9sVk"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(214 57 185 195 26 211 206 221 218 119 69 209 158 130 253 226 114 168 109 49 248 230 194 3 238 213 59 203 112 94 159 245 141 101 159 142 157 102 170 4 146 218 84 90 57 146 148 236)
      #t
      ())
    #(330
      "point with coordinate y = 1"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ImGyv2BcIvLzrvYzhxmyxIY4itUkBxmlJXMVlp7wG6J_ChBMiXBHc6gf2r7mq1x4")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(201 35 251 13 75 36 233 150 229 224 213 223 21 29 60 38 177 246 28 5 177 123 127 179 159 200 89 11 71 238 175 243 71 9 246 247 50 137 35 189 202 247 232 228 19 215 125 220)
      #t
      ())
    #(331
      "point with coordinate y = 1"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "05w4qvoJUjPvGvjA9KrzV91oQTG0rFilNPPZRGGWtgCSBy3H5TgQa3suZtwzysco")
        ("y"
         .
         "OhsgP8pHgo5vegmvZFrw9ymALKL0AAxLl3OwjFr0DiAaJ0k6s-upytfaFX-J8Uos"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(175 57 4 248 59 154 14 149 238 11 134 176 97 85 153 58 229 152 209 72 152 54 110 93 207 198 67 222 157 90 183 47 197 251 239 23 208 168 251 54 235 253 220 228 110 93 68 85)
      #t
      ())
    #(332
      "point with coordinate y = 1"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "htpXhWd4jqPMDyqdqNE5PtOkRHp1wY6oavnYSwysw7A3SRmYkO-OXTGI1PHQQdRu")
        ("y"
         .
         "QzrNXZ9VQkOesjhOwd4IR9BWBY6pJImnTgg__W4DDQYPrWdPaiqafCGYuL6eRluu"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(17 36 190 221 76 83 138 231 18 147 8 95 44 149 207 107 19 207 231 65 242 29 98 193 240 42 30 145 22 103 213 52 139 22 216 175 181 68 2 26 205 195 203 116 72 212 211 88)
      #t
      ())
    #(333
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ERRH0kL2tCHKkfVh0EsvHS8NkhLZGLtKD_sa5kWdB44hoxKyoSWU_Ix4_pRMgdUT")
        ("y"
         .
         "08mB1enNoGxDgNGoQspbtR7TpxKwSd2jpQzhCp-9C2nf_6_weHx02DI3upxGhqAe"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(105 66 97 88 162 153 113 239 68 177 23 131 239 85 158 244 41 114 70 97 28 153 77 72 21 99 113 40 134 157 95 106 2 2 99 47 233 181 173 124 54 91 99 5 12 159 45 8)
      #t
      ())
    #(334
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "181QJMRIHifRZTVmFS77oyhLULoqFBsmQ__aGPMA-mS4GyNLj2Rsq0cotOepcCTX")
        ("y"
         .
         "aFCuwKLRMcas1bKYtvkFzboY5oVkvDH4gv7VGyh4ai5y-IEmaMl2-d3AQmlVlfxv"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(101 151 64 28 131 127 180 152 239 7 128 163 130 222 205 74 13 213 128 225 175 95 36 196 74 193 100 226 86 164 136 74 213 189 47 177 246 201 100 104 44 176 191 121 226 199 166 198)
      #t
      ())
    #(335
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "4xSHVIA3wEI-VxD8YOdGq25HNSVh5Ba1gxI6bUXKqRF2LNiKYLzleymi7BodyX_B")
        ("y"
         .
         "vUtgQDjmrAFtvdSPDmvZ9LXarY1o_CiDjGq0pOtasOrqLL3vph3Fej4ErnEkKTi2"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(137 65 29 156 129 73 185 81 250 13 91 234 30 20 27 223 194 190 126 213 109 94 97 71 57 53 120 144 22 191 210 218 246 164 10 199 191 91 27 12 12 2 205 58 160 178 217 95)
      #t
      ())
    #(336
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Hhfhc9N6uYOCg_hPJKCiXKqmDEEfv-pYLET4-6JWDs4CjL45ew7aafyHLYDZ6-Bl")
        ("y"
         .
         "eJwHI-r8yemiutQxsRyvfF11mMqZkHQFa6gIpdB5d8Z4-VNN4VL_2so_Y8uj-XiU"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(111 10 161 26 183 74 229 3 78 35 30 242 99 11 199 108 218 110 237 38 50 93 18 54 66 252 230 169 80 181 15 144 75 192 38 159 143 88 155 196 211 129 7 51 43 20 218 132)
      #t
      ())
    #(337
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "du_MgyLwr41i5zag4RD2azqtXIe_enczP86GT0um6N_cFtat0sVCxIBViacfDkij")
        ("y"
         .
         "wmj-qwJxTIkvgmYislvSHOKjNRSxbJ7oTHIrUjsZ7F_DTYdBMwgOFYmBhJNV4fUN"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(126 185 150 46 4 73 204 210 230 11 79 128 148 213 167 180 97 208 212 176 106 240 186 48 107 116 171 13 96 173 92 248 148 251 252 40 42 200 220 163 218 25 109 122 41 107 170 20)
      #t
      ())
    #(338
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "wUfWwjXXQLOI5tKvr8-F1bcWbg306iEpQIjjBJtcJ9djPJPTD9zyuTdeGBT1G1e7")
        ("y"
         .
         "77LBJXXCI07h9rFHsuBMEEpNKynNhFj1m1e_91yrm2rL6FT8TZb7czctYFS7doOQ"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(121 2 149 190 214 159 83 71 236 170 233 190 254 21 205 52 153 117 156 223 190 81 151 75 31 28 75 197 118 231 216 129 206 250 147 213 232 191 44 33 235 185 130 105 142 28 122 235)
      #t
      ())
    #(339
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "hJyY__XjGxCK77Fsxc9cfohDnYs-8YjypbxT8wkB3t_ilKuzj7jLRlR6I8_keQnW")
        ("y"
         .
         "VkSfigmqiPjiYFmOonct900mpEMaA3hZNDc757DskYe4IVlldhcj55WRF2lg6sAU"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(88 161 26 23 196 173 164 212 70 192 139 131 170 5 104 127 238 143 214 231 131 3 100 139 133 131 38 33 151 248 123 233 125 190 11 189 39 78 68 74 56 78 34 22 150 83 15 201)
      #t
      ())
    #(340
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "V89kVXjdJCxxTRAN0jVosW03RdqAlwjQHranFo5vcd7CB0xabCO9BVI9_MyX3qbw")
        ("y"
         .
         "jaGV2qIFbaBXa7wP-15zu9rQOwTSO0tgJFGlupGxfc4wL0rRZMakorWOPwJTpenW"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(146 96 72 58 22 86 144 213 76 16 203 134 122 159 137 107 189 18 61 71 104 161 141 50 35 0 212 32 170 200 138 226 15 24 139 87 248 233 122 250 213 94 187 129 87 40 244 154)
      #t
      ())
    #(341
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "yrLZ02yOwJ8YuqCrtSE3KlzC9iSEj5VGCoYVI45owRwbQ9IrhNHlNewdhVYN4pPO")
        ("y"
         .
         "H9Xm56x-tvFzrSe2OYX8yDuVMqpW8rWbU6nRFgmP2pq6SdV8Hu0PBjvMcBuxD2wr"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(180 103 154 248 10 42 211 78 248 110 172 242 194 25 21 86 188 2 64 199 145 63 177 76 76 31 134 35 181 219 65 184 84 214 48 220 101 115 141 31 175 84 74 164 41 121 174 208)
      #t
      ())
    #(342
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "1x30CVwThSM0CkAQz6l2s7J69IyoPoxrtFlvn46PE-ON5OJc4dN5tBqV-df4qEYG")
        ("y"
         .
         "soxpcEtoeI62o1yLyqfZcY4AybxWes3EWLZzKqhwQdJ7aldNUelUN4Vb3IZ0jFys"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(138 52 222 139 235 136 67 100 181 131 201 203 206 57 9 105 150 124 194 165 0 144 238 19 42 203 212 155 247 128 173 127 9 42 227 235 212 104 106 112 186 85 78 253 214 172 212 237)
      #t
      ())
    #(343
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "pXYXdYUKJZhaMkMEQtZgqt-eHjiLV-bhv-QUlbmgWoJA938zl8XNsMg9c5RZhKvt")
        ("y"
         .
         "0sfPpdXZFzjGV3ZGZYwESPAVW1v4OVdaNtGybYfRsrOvtPaGU6a_pe38V6qFKeYi"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(121 13 252 255 71 33 239 23 1 227 255 82 30 66 33 190 81 174 36 47 149 22 107 146 146 199 170 47 163 201 183 153 175 211 83 152 122 83 86 144 255 32 214 111 87 143 56 252)
      #t
      ())
    #(344
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "VPwvic3m9uhrbPMoYZj5qeM8jWYsIVEgXdz_EuLTL6iR7sI1m8ByJNZMXjOf-dWJ")
        ("y"
         .
         "GKO3k3LRQvs0HfOxW1SzDo-VawebEngnajO02FCJIJQMpj-46XD_mPkezHmGJ17b"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(203 134 16 83 193 214 133 25 78 165 132 19 216 249 152 30 7 20 91 197 115 186 146 75 219 162 226 156 95 152 233 231 14 160 205 4 18 214 195 135 68 158 154 231 103 220 67 48)
      #t
      ())
    #(345
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "v78_qxrLOc4o8GPXjPJQdyvQC9ypi3aeHx_UQrd_QnAQal99kTwZBb6g3HWzH4eh")
        ("y"
         .
         "FcVm2_8M0QczJUUKkQlv5fXWAaY2QHz7xPVyDzlvpssWmHNwznDim7ftL0R0JfEK"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(55 95 251 144 173 108 81 134 104 229 210 89 65 172 213 249 192 23 52 109 6 248 147 207 149 117 34 88 188 197 249 162 89 150 168 15 66 224 71 90 128 68 61 196 25 123 186 108)
      #t
      ())
    #(346
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "V1dXdIDzKu2sbhl2X5GXzCeMsLGiH8a2QjyPbGzeodMkyT7qi9fcyo_Sk9aYdlJD")
        ("y"
         .
         "YLGkglUirNgz0429YVCFRzWKL1woG0lqvVcHKrFPmiI1vKAlReBCdSa8K7ySaDSf"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(209 160 42 38 74 177 90 239 82 165 52 235 80 0 226 148 61 140 241 247 114 85 162 127 132 94 150 225 248 29 244 126 233 3 195 233 33 83 157 119 215 47 85 183 239 201 223 187)
      #t
      ())
    #(347
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "FFL16vu1zFY_LHykA7gD9vjWZHkGJQiiJYqNmIp2m4Dgd3NbRe-rHyl87r10NH9Y")
        ("y"
         .
         "TyutvKVUV0BIZrrvq_qZtw-_EF0X81I0f2eZZHRIcI94j4oUW92W1K8tfmVL5-Xx"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(204 170 55 118 157 3 81 112 187 92 25 193 53 218 34 145 68 110 253 126 205 59 233 0 134 18 184 204 194 159 110 157 122 69 221 78 4 121 18 63 3 231 227 4 243 71 41 233)
      #t
      ())
    #(348
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "rSejZeS2mUxe0TX8R5TXPrcewm14f-wQ8TgrGcG1zR1nKZn-7obV8wVRbWAf0caq")
        ("y"
         .
         "-GZIWJiPAvNsiI9xhyAHhwuzJlSqdVMPw7eiNff9K82BXjA0RVJpYPb_zdYdLRNA"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(74 233 77 62 218 88 57 244 184 185 255 230 213 16 20 76 242 139 222 152 88 30 130 114 111 45 118 217 51 145 154 218 183 29 44 223 76 67 16 248 171 204 116 215 14 186 58 51)
      #t
      ())
    #(349
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "czBB86YEaLwHEdWlBzpx72Bi0K5wcGr1ExYDNYDK4Fj8Ssmeu6gjsZ9I8wnjT7Ap")
        ("y"
         .
         "M2cNzHXtR3RHcfYCOMzrzdzNQtZOQQRf2OvDW7rgo3tDOI3bhzVsu1JaE9V_R6LN"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(6 43 138 150 142 69 67 92 43 91 84 180 23 245 94 137 249 53 182 109 236 68 136 44 6 171 141 108 218 141 207 181 83 167 131 97 211 130 169 33 17 145 135 30 154 52 34 15)
      #t
      ())
    #(350
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gYhYBO6TGYjP8_IYya8UKVc8z4yPPfQMaY2XbovHwSiBlTRF3cTxkCsvLzFTME6C")
        ("y"
         .
         "1wlLZzt2vuv6XYTEmwHOFoxFkgHLgZLvltiRAQ5LyI8KzE-7QK1hlm54e5opiiu6"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(186 65 195 141 51 178 174 52 39 151 238 105 114 63 4 42 113 221 127 177 95 61 200 13 174 64 61 79 136 172 174 40 226 229 31 44 169 213 43 74 164 226 179 229 130 87 226 77)
      #t
      ())
    #(351
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "FnkV2WKkBfjQTW1klCcNiD_nWU6aIfe1dxV1D4147AcU8PuFqf5a7k5tEDROSxL5")
        ("y"
         .
         "HwBEHIv1Z_-E5u5CmLnTOSx4-zJGGVgQLh0p0vTEcyEEJCzEv-4iqo9KFyqlEKyI"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(198 175 78 174 89 153 224 208 101 133 5 203 75 57 94 157 115 89 42 61 157 11 60 7 202 235 43 239 27 3 202 77 96 59 51 230 246 187 217 15 148 22 196 170 0 34 229 132)
      #t
      ())
    #(352
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ZuH-iT9y_-peJOpaTw1lWP29MiLbpuM0B1PIKXmsWVMNvTf6wvUTVUZdVq_h3lgr")
        ("y"
         .
         "6sqRBK0mmw-EjYtNcRgq_DASkUc2mnQZPd9N2ztv8Sj5QWjOC8YOd5G0scouKUMb"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(158 54 74 220 183 43 21 164 44 189 111 167 145 20 136 158 34 185 59 83 59 1 25 175 217 9 20 177 129 18 129 81 184 161 125 180 144 4 236 57 173 93 181 96 63 185 113 29)
      #t
      ())
    #(353
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Yuuqv_KgeoUq-aq4Em6TwxEJ6JPMYbNUpyoXRMtAmgLOxuYZWEWVelO96X3seofC")
        ("y"
         .
         "XqEbADppTjccK-HPXbW3-6J49paPVMX7ZneYe7trVd2QcugyWzZcJSYsUCT2iy5b"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(35 16 72 6 219 216 95 113 216 252 56 16 27 12 208 187 205 96 51 29 143 116 134 159 6 67 17 106 195 98 181 233 51 219 135 67 255 128 223 204 45 245 203 203 240 39 24 94)
      #t
      ())
    #(354
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "J2paJ0tGoKNvgRz2S8emRNWtnBJL4AKm_dHMERiAZVyNKYdnTqSefXDSoK6CDRp9")
        ("y"
         .
         "TsO64t0dd68IL6vNaKwg8gno-kRkeHlCI74dY9cZh-lk5F8Lz2ouoKEsYzRFKFpG"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(40 53 2 53 91 33 24 159 115 97 232 148 87 85 9 116 248 106 226 182 109 2 85 6 137 156 21 179 144 218 36 142 20 253 27 79 37 200 88 77 26 113 231 248 201 63 169 118)
      #t
      ())
    #(355
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "mP7jFM-zwBzC2f18JWgzYCeEAAi8-_yCkkIe1rByf6ILcAKlcmpoXUSWOtj3TGld")
        ("y"
         .
         "olpUK44LuaSFi-ZPcIANqTr2d2u3lbsWn0ydFReV86WjejyCJkz2hwxN7WhKcSab"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(144 68 190 112 86 102 100 237 145 218 184 90 165 255 100 77 122 52 104 16 210 103 141 39 117 25 23 195 130 12 76 130 80 52 183 138 149 123 143 209 212 126 134 230 126 91 169 248)
      #t
      ())
    #(356
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "0W0LEdCjJrsnJbPJzVmHRq8Rkk9hcs5TsZqUK5ZVYsKih1_RXOH0hpFW1c8IeAx0")
        ("y"
         .
         "1MSSfKF1tTJ8nzIc0qexrLWPzjURYNrqXVxk120406sX98FDIJOtTWIAU_b2dYZK"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(31 164 200 230 222 200 144 136 168 254 114 85 48 245 193 84 103 46 132 72 139 170 247 10 8 178 233 101 101 150 38 83 87 116 2 22 20 36 41 75 127 55 114 8 252 147 97 57)
      #t
      ())
    #(357
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "EFJuOwl6mepQe6Z5XqH3dxhpBGARe_rJ7ZW-4xHbX4oaJ2fBNLGds8OHoftFvnAp")
        ("y"
         .
         "MSx117vu7ki8FbVd-3FlEcVjTXGdJpA7ys4cYKjgMElhVtnMrJfKxyTgk1dK5LyD"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(4 244 2 157 55 35 50 180 178 38 201 243 37 33 234 57 165 139 101 93 36 227 81 144 206 4 213 143 12 4 129 75 231 246 255 203 138 74 21 19 66 36 19 79 2 158 218 159)
      #t
      ())
    #(358
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5jLLIBKItkPyNVM8RIRrEaKHQO5BN6Is_39UIFTWM39GNjhJyDEOINiym63bMQn_")
        ("y"
         .
         "lI1CrpWcOuFD1PAtJocmvCtmuv7XZXsaU-I-7UgJTFyo0o0WEhqmI32BUK1sMmZV"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(244 31 145 33 101 130 124 234 39 171 129 71 164 106 190 179 12 27 199 60 254 5 32 215 216 105 141 248 249 82 154 254 38 234 199 7 52 15 56 180 158 197 231 65 214 70 185 167)
      #t
      ())
    #(359
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "CIMwOAYTrbC45TFkmAn76jQO299ZivAo50ugtPGOLVARhTG5qSsHdTssE864fTyO")
        ("y"
         .
         "OOYltQD0ub0tx4GJL_-W2JJxkJ0-Vhpj5BVGfW1FXQpMGcNvFtzCQXo0JcKt2pA0"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(110 67 126 38 49 110 178 89 100 0 216 253 218 91 233 12 170 148 214 44 66 95 39 223 92 166 120 152 168 255 223 60 222 87 124 77 45 102 246 82 36 152 240 178 149 215 122 46)
      #t
      ())
    #(360
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "2anR4KrK70rs8kHWkO5BAd9016N_GM4xgRKPjUJVzTRwKxfxFrJQbLj1d9k1fjYp")
        ("y"
         .
         "lm5i5vlcnreDPMwfIpVqHiWZqHX4E52aKzHiAAYRtNbbHSPn8NtbXBe7ROFQ6iaV"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(133 71 114 185 183 125 70 135 87 31 25 52 6 29 188 224 1 109 71 185 10 139 93 56 34 148 117 194 19 205 93 242 95 150 11 119 90 231 41 9 204 204 91 161 77 208 52 72)
      #t
      ())
    #(361
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "U10i85yKVZHnod7vemw0ll-10eYW8UuxKn5jENUgNQ8vdYNvnIGJ7EUD73SguFDU")
        ("y"
         .
         "q7BaseXGBhiE0e_z0r54x2dZuYbd5wVSqh6XvAFPMzwNWvi0bWHhrgZ3Ti-GJnMS"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(3 125 69 154 84 254 107 105 216 147 244 211 9 74 94 10 147 37 29 82 84 206 245 176 34 231 6 95 223 82 227 190 129 39 242 174 46 51 110 38 26 113 246 228 26 172 178 225)
      #t
      ())
    #(362
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "iUAqFD1X7Fha7hJD06et-ofrgMYL0Jy7VrZOsKG39ZJzGNhXPGI5xv7qpY3Cri5A")
        ("y"
         .
         "1gUYmsSma9a55wi9vMxGR1Q8DdHTxkosFgwy9ynysV3ZWFLJsgdQhlHXdGOVpaSQ"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(232 208 61 255 234 210 228 14 169 21 219 76 156 103 171 180 90 166 9 146 66 43 112 55 66 57 202 245 139 150 248 103 22 96 12 28 196 68 147 98 220 86 202 5 123 252 94 94)
      #t
      ())
    #(363
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "wqnFKlk2kQwFxBRDFRJ79mLCkrjwaBgRwr-PT5zFvXq2k_fL3bVyTn4p3QI9FV_9")
        ("y"
         .
         "M16Lhyrht6kSk66cyPZU8F_0dn2yCLdsc7azTFYmUS5rqSvMk3ybzKlEfb7CL_EG"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(232 32 249 151 146 138 113 24 108 224 155 150 146 60 49 124 193 40 203 118 28 135 233 112 115 168 81 97 12 161 47 33 153 55 147 135 137 189 34 122 141 114 178 180 6 196 113 253)
      #t
      ())
    #(364
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "3NAiStIhncOvBe6l0nl0p1-xlg0Nchw58Qe_RTRiIJlOUgylpkZwHpMZzrCAF6MC")
        ("y"
         .
         "4LG-4pLNHLmfFF6tPRIoWQOx5Un8znWRoQGhZ1-5tuMOZ4A39NWNIgweNqTnn4g8"))
      #(("crv" . "P-384")
        ("d"
         .
         "wXgdhsrCwFK35PSM70FcXBMwUvTlBDl-deTXzQyhSdoLSYi4pt7Vzq5LWAaRN2GH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KeucY6EjIs2Z3g6OPBHmyKSGsQCBgb5Mgl84s3Hm8D4pJs1g6anM1C4aqHmdLvOn")
        ("y"
         .
         "nQRfOj35dHQyaL5lD7r2jHBFpIMJPOLiiZAwlzlvBABY-qy3I-tJO1tjG8NlxfxJ"))
      #vu8(10 237 138 199 208 74 240 112 183 58 3 243 126 247 17 41 109 184 172 100 186 177 62 21 65 141 246 55 58 173 129 216 224 250 120 155 146 146 147 61 127 17 184 97 64 118 224 116)
      #t
      ())
    #(365
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "B3pB1GBv-hRkeTx-X9x9mMudORAgLc0GvqTyQNNWbaa0CLuuUCZYDQLX5ccFAMgx")
        ("y"
         .
         "yZX3ygsMQoN9C76WAqn8mYUgtByFEVql92hMDtwRHqzCSr1r5LXSmLZfKGAKLx3x"))
      #vu8(69 90 234 153 36 51 11 214 210 214 64 52 120 50 121 0 225 114 233 53 152 226 84 207 109 142 177 63 10 61 33 190 81 164 97 7 51 56 68 230 29 250 61 128 223 105 40 233)
      #t
      ())
    #(366
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "AAAAAP__________________________________________________________")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "r1HxihVijVYSyPcUQ275D5DVoLo1BU6ZxXRbsQ0L8xjMgsD38sEZ4Iox4_zKW5_j")
        ("y"
         .
         "HP-1ZchnGMWeHmpwpleQdon3OnET41__WWhh0-L67nCSm7E1dODhA1aUQgzXPgz1"))
      #vu8(219 29 142 241 17 114 130 135 13 184 17 58 164 245 135 35 199 86 206 89 134 134 235 142 165 49 170 77 57 171 177 185 130 177 231 187 38 72 166 194 104 210 211 81 32 77 184 213)
      #t
      ())
    #(367
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "AQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KEG02v8RPTdX_8rK2HpKdfBxGi_9I1Ke8fM_ynVmW0moqKzOVBOJSk0V6z20Hzcu")
        ("y"
         .
         "fEUl6RpTGNFxhPn_Up-j75atipUVVQ5YHC04kJz8TX7CWC202z4bd8IYFZDJ-rJJ"))
      #vu8(233 128 98 223 71 239 136 76 148 17 225 100 102 175 132 173 39 29 88 96 8 177 251 197 10 235 59 54 131 106 53 167 112 221 66 224 219 132 211 155 38 244 220 210 220 3 217 11)
      #t
      ())
    #(368
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "f_______________________________________________________________")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "0T-b0MBbVsww9gEGpLjlAGk7nfBZac9HSU1dLBaFTvHYaSs-CbgJR_c1cQdhbdyz")
        ("y"
         .
         "uzx-gwbEb8OYxlIbZDRwwld02tg_r--1xkHP7K3sv5Sz1t1Zr3ruhqmJOQfWgZgU"))
      #vu8(137 138 174 14 191 28 180 159 182 177 35 77 96 245 144 6 50 84 33 4 154 138 50 8 32 225 173 106 246 89 60 220 34 41 160 140 80 10 165 92 160 89 153 209 40 41 219 156)
      #t
      ())
    #(369
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "gAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "4-Jc2xYCCLZHTis01yv1hruhT3LD-X9RWkBdFCkZbmZzFht4rYCv5mTuUE1LFhq3")
        ("y"
         .
         "N3C2TVRCaVlZ-4nafrOnyvy6B50yAx32ITBJwcxQnj-RIMr43ZEJEV9AOFmsM3rM"))
      #vu8(131 248 98 244 150 171 138 241 43 130 168 160 192 71 216 54 189 250 54 40 19 36 179 161 235 46 156 29 70 105 157 129 203 18 92 190 75 147 147 159 216 78 26 232 109 138 131 203)
      #t
      ())
    #(370
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "________________________________x2NNgfQ3Ld9YGg2ySLCneezsGWrMxSlz")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "B53FZlEMxxj_NIeX4s_mfC_Ok79fbzm_dMGsPYy64--S3Zze36D1bQa6WRjXTZZC")
        ("y"
         .
         "WbJfZaJveEN2V7yBo3-Qd52ciI97e0oOydiqx3bbplXWC2KuM6470F8TKjA-lErc"))
      #vu8(154 38 137 72 135 160 52 44 165 89 167 74 77 74 142 29 107 32 132 240 46 28 101 179 9 113 33 169 169 175 4 125 136 16 251 148 93 194 91 191 2 34 43 59 98 95 30 10)
      #t
      ())
    #(371
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "________________________________x2NNgfQ3Ld9YGg2ySLCneuzcGWrMxSlz")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "2ul25L7jPfzrNGjph0zQ1HLI2rnst1ODT6Y4hMczqjhgc7sWNwblrLUAS13O1-XO")
        ("y"
         .
         "i6wBz6smj6QSpGD3AHV5Sl7eVsmhYPEzFzroaKJZGBYM1GuUHNy85AngsPTXAnsi"))
      #vu8(138 141 157 193 148 162 105 16 203 218 231 144 141 24 91 106 208 75 98 12 148 197 238 51 30 88 78 216 4 228 149 190 188 34 144 162 215 0 106 6 230 91 155 202 206 134 198 246)
      #t
      ())
    #(372
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "________________________________x2NNgfQ3Ld9YGg2ySLCneuzkGWrMxSlz")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "pxs13OBdGmF3ZPsuJC1iunQshrE5I-iFZktg5Q8-VqRT6V9OFDYLwckAKqsdRacj")
        ("y"
         .
         "xpnhNP3vFVPuNWIOeUEOU2lr_U64di40Ngpn3K0L87rRj5Fg-PrGfoG7dGdJb7Gd"))
      #vu8(213 127 106 161 45 63 7 232 149 132 153 242 73 229 44 251 229 190 88 72 46 20 108 84 20 219 191 152 79 197 51 55 16 53 14 44 233 107 51 190 183 103 131 129 244 15 29 203)
      #t
      ())
    #(373
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "________________________________x2NNgfQ3Ld9YGg2ySLCneuzsGWnMxSlz")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "uiITx61iuJVicr7rUdJYoqW-gE4OKe7RYaFiFu6J-4pFNGwthEOayBrDudZ1737O")
        ("y"
         .
         "VwJGw7uSeWHjgSMz76lMfW7Nu3EgoOE-uKYex9khVHElSl8-cfqiD_3cudv81_tk"))
      #vu8(24 142 128 65 217 165 240 182 207 218 211 21 173 164 130 59 237 160 20 103 116 250 214 91 80 14 110 249 67 118 235 248 175 122 64 255 111 107 69 1 154 9 221 231 215 251 85 82)
      #t
      ())
    #(374
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "________________________________x2NNgfQ3Ld9YGg2ySLCneuzsGWrMxSlZ")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "iNrpbcdquMsriNNj0fzuxqQ3PHHJA5mnzruyWgd5LEMU_N87yFZSdpws1Iu2Um6A")
        ("y"
         .
         "Qok3vbvwXLRErBLt_vNenzKaO7llj1ZjlYvs6zSe5zFee83A8ztVb4b55650Wmhl"))
      #vu8(46 207 157 196 126 139 7 174 97 221 189 22 128 234 208 38 152 233 232 70 159 120 213 162 131 40 228 141 12 157 122 42 199 135 229 12 186 88 204 68 163 47 177 35 93 45 112 39)
      #t
      ("AddSubChain"))
    #(375
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "________________________________x2NNgfQ3Ld9YGg2ySLCneuzsGWrMxSlp")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "pmnFVjvWfuxnjSnW70_ehk83LZC3m56Ikx1cKSkSOMztjoWrUHv5GqnLLRMYZlj7")
        ("y"
         .
         "VndI1Rg-2GDdJvfCSg8TIgj-5qrz58POOv0ghzxI-lbWkn5p2313JmiHsJZIxd4i"))
      #vu8(6 238 159 85 7 157 61 60 24 198 131 186 51 224 210 82 27 233 124 79 191 121 23 191 59 98 135 213 143 252 222 45 248 136 66 227 245 83 11 57 84 154 194 9 116 177 182 14)
      #t
      ("AddSubChain"))
    #(376
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "________________________________x2NNgfQ3Ld9YGg2ySLCneuzsGWrMxSlw")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "B3pB1GBv-hRkeTx-X9x9mMudORAgLc0GvqTyQNNWbaa0CLuuUCZYDQLX5ccFAMgx")
        ("y"
         .
         "NmoINfTzvXyC9EFp_VYDZnrfS-N67qVaCJez8SPu4VI9tUKTG0otZ0mg16D10OIO"))
      #vu8(69 90 234 153 36 51 11 214 210 214 64 52 120 50 121 0 225 114 233 53 152 226 84 207 109 142 177 63 10 61 33 190 81 164 97 7 51 56 68 230 29 250 61 128 223 105 40 233)
      #t
      ())
    #(377
      "edge case private key"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6d-qq4CLOqwczKfMYkKn7lgySa_o7o9muQTMjuw0rTNEVuAPM6lN6LUWnPAZlVDA")
        ("y"
         .
         "IBVullFzT_mZxfPqYrg9AIOmCT8jRFclHs9yxB5N986iQgtUVKf2kANDgLrJgeku"))
      #(("crv" . "P-384")
        ("d"
         .
         "________________________________x2NNgfQ3Ld9YGg2ySLCneuzsGWrMxSlx")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "CNmZBXuj0tlpJgBFxVuX8IkCWVmm9DTWUdIH0Z-5bp5P4Ohuvg5k-FuWqcdSld9h")
        ("y"
         .
         "cX8OBaTkwxJIQBcgApJFi02KJ4pDkzvBb7GvoNqVS9mgArwVssYd0p6v4ZD1a_F_"))
      #vu8(2 76 82 129 72 114 22 5 130 112 205 28 254 37 158 148 131 16 228 173 194 99 169 237 170 77 160 188 63 95 140 232 255 200 138 228 27 44 5 11 246 221 156 140 102 133 114 55)
      #t
      ("AddSubChain"))
    #(378
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(379
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(380
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "__________________________________________7_____AAAAAAAAAAD____-"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(381
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "__________________________________________7_____AAAAAAAAAAD_____"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(382
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(383
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(384
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB")
        ("y"
         .
         "__________________________________________7_____AAAAAAAAAAD____-"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(385
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB")
        ("y"
         .
         "__________________________________________7_____AAAAAAAAAAD_____"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(386
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__________________________________________7_____AAAAAAAAAAD____-")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(387
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__________________________________________7_____AAAAAAAAAAD____-")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(388
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__________________________________________7_____AAAAAAAAAAD____-")
        ("y"
         .
         "__________________________________________7_____AAAAAAAAAAD____-"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(389
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__________________________________________7_____AAAAAAAAAAD____-")
        ("y"
         .
         "__________________________________________7_____AAAAAAAAAAD_____"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(390
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__________________________________________7_____AAAAAAAAAAD_____")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(391
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__________________________________________7_____AAAAAAAAAAD_____")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(392
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__________________________________________7_____AAAAAAAAAAD_____")
        ("y"
         .
         "__________________________________________7_____AAAAAAAAAAD____-"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(393
      "point is not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__________________________________________7_____AAAAAAAAAAD_____")
        ("y"
         .
         "__________________________________________7_____AAAAAAAAAAD_____"))
      #(("crv" . "P-384")
        ("d"
         .
         "xsr7dOKlDILHpj0TKUv-oT0LxQS6KwijkskIG_OBXZ5E2WntfwX_0dhZQ1UFPGFH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "XlPoNhRrUZZ-dcYjR7QUbnnAVhCsYYuV2aYULAsj-80-OvhlSto7v-gegKdXrBYj")
        ("y"
         .
         "2tXCeTqL3j6KR6vboyV-QccHR8iWe7WU4K9dO5f6EegvcfIpz0z_O1zIW12oCQ5w"))
      #vu8()
      #f
      ())
    #(394
      "public point not on curve"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ISGjSPl0OFWFnElvkdDzn-co_EbkjQB3EwUbIvHAJX_iDdhbId9-Hsgr-LObITii")
        ("y"
         .
         "rnT4DmJXd4-Myp8nm1fSXu6xVZYGQpcvBWfiBFFPCsHrHifbURUFMhGRSWHQlkTI"))
      #(("crv" . "P-384")
        ("d"
         .
         "3kTmP9kk8Xc0DXgK9qquonH1LSy5pcUZtgIOBsPPC6r7wLgBxlCMLhSDsVz-96_C")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Zc5rnX5x6TG2_R-skQUHtDd6wV5FMpKiNV3zmrdByjYpe_bePDSlr7utNxgGuJHy")
        ("y"
         .
         "SwiLxdFGRqdPPvz-o54S4rX1brvdUD1ej-4_wwDXUJuqIrZo2_H9JBgHCrQKB9n2"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(395
      "public point = (0,0)"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-384")
        ("d"
         .
         "3kTmP9kk8Xc0DXgK9qquonH1LSy5pcUZtgIOBsPPC6r7wLgBxlCMLhSDsVz-96_C")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Zc5rnX5x6TG2_R-skQUHtDd6wV5FMpKiNV3zmrdByjYpe_bePDSlr7utNxgGuJHy")
        ("y"
         .
         "SwiLxdFGRqdPPvz-o54S4rX1brvdUD1ej-4_wwDXUJuqIrZo2_H9JBgHCrQKB9n2"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(396
      "using secp256r1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y_ZgZZWj7lD5_OqieYwnQMglQFFrTlp9Nh_yTp3RU2Q")
        ("y"
         .
         "5UCLLmefnVMQ0faJOzbOFrSlB1CRdfy1KupTt4FVazk"))
      #(("crv" . "P-384")
        ("d"
         .
         "3kTmP9kk8Xc0DXgK9qquonH1LSy5pcUZtgIOBsPPC6r7wLgBxlCMLhSDsVz-96_C")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Zc5rnX5x6TG2_R-skQUHtDd6wV5FMpKiNV3zmrdByjYpe_bePDSlr7utNxgGuJHy")
        ("y"
         .
         "SwiLxdFGRqdPPvz-o54S4rX1brvdUD1ej-4_wwDXUJuqIrZo2_H9JBgHCrQKB9n2"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(397
      "using secp256k1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "oSY-dbh64JNwYP8UcvMw7lXN-PQynWKEqev7zIVsEWg")
        ("y"
         .
         "QiXnLL6_9B5U-28A4Rr-U6F5N77b8t94f475WE93WDg"))
      #(("crv" . "P-384")
        ("d"
         .
         "3kTmP9kk8Xc0DXgK9qquonH1LSy5pcUZtgIOBsPPC6r7wLgBxlCMLhSDsVz-96_C")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Zc5rnX5x6TG2_R-skQUHtDd6wV5FMpKiNV3zmrdByjYpe_bePDSlr7utNxgGuJHy")
        ("y"
         .
         "SwiLxdFGRqdPPvz-o54S4rX1brvdUD1ej-4_wwDXUJuqIrZo2_H9JBgHCrQKB9n2"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(398
      "Public key uses wrong curve: secp256r1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "X6T6CyNcIeXJ87rqkwO_huzLfTHQuZjhQbxUtdxDsj4")
        ("y"
         .
         "73_Fz1YwjtWV7uma3mqvdNWRw9AKobQ4q8WclgfCLDY"))
      #(("crv" . "P-384")
        ("d"
         .
         "1jMaWpaOTTvXM2pCO0EFW2jt0QC4uZjQDrntk4gcIeORK7LuCOcTJ74gWJhnXvek")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Lf93OogynmDUoGvzJrQTg6RSsTuMgHRexwG5xH4RThYRU4AmWcVyhAtW96p35FwW")
        ("y"
         .
         "LObo-hlEr8xFWhF_Id6Rz3sPPU6DoTziqqXlsLnvQzIqhN6zEXy3eEqG2KGGvLFb"))
      #vu8()
      #f
      ())
    #(399
      "Public key uses wrong curve: secp521r1"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AFvOYf4nxED-261H2IvM9kXbnB0w2qCG5ZLotqChc7h5kbYZgBkHtCD6VYx5U6uXut2cbB2FhZ2evvdEGgiP9X7V")
        ("y"
         .
         "AI12ON5wP6q-taeOg-j81Ot4YUSnXXm9TMjPqL5mYS11bHtlxn9yxqy63m8NWel1LoRSBbKlYNT41qnoS_gS-U0Y"))
      #(("crv" . "P-384")
        ("d"
         .
         "1jMaWpaOTTvXM2pCO0EFW2jt0QC4uZjQDrntk4gcIeORK7LuCOcTJ74gWJhnXvek")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Lf93OogynmDUoGvzJrQTg6RSsTuMgHRexwG5xH4RThYRU4AmWcVyhAtW96p35FwW")
        ("y"
         .
         "LObo-hlEr8xFWhF_Id6Rz3sPPU6DoTziqqXlsLnvQzIqhN6zEXy3eEqG2KGGvLFb"))
      #vu8()
      #f
      ())
    #(400
      "Public key uses wrong curve: secp256k1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ppztEai_epB7-kfLozaPJJi0ZaJAfJBknI2iJNKoW_Q")
        ("y"
         .
         "Ra0t89ARPnKu3M-SumuFKe1vqhVLwnq6JfSTcZgeOzg"))
      #(("crv" . "P-384")
        ("d"
         .
         "1jMaWpaOTTvXM2pCO0EFW2jt0QC4uZjQDrntk4gcIeORK7LuCOcTJ74gWJhnXvek")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Lf93OogynmDUoGvzJrQTg6RSsTuMgHRexwG5xH4RThYRU4AmWcVyhAtW96p35FwW")
        ("y"
         .
         "LObo-hlEr8xFWhF_Id6Rz3sPPU6DoTziqqXlsLnvQzIqhN6zEXy3eEqG2KGGvLFb"))
      #vu8()
      #f
      ())))
(test-ecdh-jwk
  "ecdh_webcrypto_test"
  :algorithm
  "ECDH"
  :curve
  "P-521"
  :encoding
  "webcrypto"
  :tests
  '(#(401
      "normal case"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AGTaPpRzPbU2p0oNilyyJloxxUodplKaGYN3-9OFddnXl2nKK98tTJcmQpJtREiRplLn9JIzclGt8WE88wd5mbXO")
        ("y"
         .
         "AOBK0Zz5_UcisMgkwGn3DDwOfrxSiJQN-pJCIVKuSk95GDztN1r7VNsUCd3zOLhbttv8WVAWM0a7Y6kKcMWroJj3"))
      #(("crv" . "P-521")
        ("d"
         .
         "AZOZgrUpWWznepS8bv0D6SwhqEnrT4e49hnVBu_JuyLnxhZAyQ1Zj3lbZFZtxt9DmSrjShNB1FhXRECnNx9hHH3N")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AfrISzjXMtrM_O-q2aF449BPLDDhnEkl0RLogjXBDAiANtTR6iUo9eAgA9pwNTMXBInGV21uHk0Pxz7sEmylWs2E")
        ("y"
         .
         "AAkEthmdL26JCmVL4gyeZaQK0SAVcEOIGCL8Ut9BIORWOnzVkFwBZ41l6EXq6cZT4wP3nFI5EFUdIS-9fCXi0TRJ"))
      #vu8(1 241 228 16 242 198 38 43 206 104 121 163 244 109 251 125 209 29 48 238 238 154 180 152 82 16 46 24 146 32 29 209 15 39 38 108 44 247 203 204 199 246 136 80 153 4 61 173 128 255 87 240 223 150 172 242 131 251 9 13 229 61 249 95 125 135)
      #t
      ())
    #(402
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AUxkMylpG6J0WaQN_nxM4Xs-oU0M16pHsB8TFUBNtRQ2-7_m3ghC4PfhJl9v86yih1BnfTNwsvsqbvSXNW9LlYES")
        ("y"
         .
         "AQUbFBeGOaCaQUZcctN0NDbuHBkf9ziKQBQLNNUxfeWRHqA827Ayn960RmlaO5LUNycanzwxiwLexNRzkIFYFA6X"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(403
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACnNMhJcI6Qa8k_UtynaD6rLw1UW7wulkJZgJXFpPNKC4m1n4Y70ZD0PbxWNc3DTOUypqN55OAMqwXjG_TTjcCuN")
        ("y"
         .
         "AIZJg04rQb46i3UQv-Vw9MZwdZQ80Mu52eHR2lJhi1uW1q7JtlDa8cpmJME-URYwK5x5yMTT01GRXR6OGratdgmO"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      #t
      ())
    #(404
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ADLG8GzmoV6gZEZNNao2jSmcmp4eNo9pSu-2A4diSPiY8iPOAhe-831h6wmyfJMYfPjmG6exTjyb7mkrBqxtlfg2")
        ("y"
         .
         "AZ_Rn4SA4hxjIR1I1F-W9jZc9V-VjhoP5-pra5_yMKh7cLsbFNOl-2ZpqRZBxqz0VwwdOp5wmRO3_ms1_4HDlNan"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2)
      #t
      ())
    #(405
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AffrluZLGmLa-eCAG_2WoLFbaOX1yz6QtDRJWkc5BzOOUwmOHC5JMzXQnGqub92gNFuYqu1Yjyq-gpEHE_tsICUp")
        ("y"
         .
         "ATlrF88lC8AY9M6tCX5-CYY_FM8SObBl5X2ISUnu4UGSb358n380zwU2NodnvA4atRQodyk6THImk6c_4UpTkK-T"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(406
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AG3fmxCWXV_BKelvejdmfM9mzEQ4R3KQb-2yH53kYp4BqqCax8mGYRIGS7yb1Y68Ejqy_hnY_tGgVtJ7_vBjBQnH")
        ("y"
         .
         "ABxEExHvIKFjRjMupC1cZXiNaPaBewJn_KsR6pyUjtEIEV3ajoI6OAtgFGB0LTdy1kJMZ7JA2iR3L_DSzNmh4M6m"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(0 0 0 255 255 255 255 255 255 255 0 0 0 0 0 0 0 255 255 255 255 255 255 255 0 0 0 0 0 0 0 255 255 255 255 255 255 255 0 0 0 0 0 0 0 255 255 255 255 255 255 255 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
      #t
      ())
    #(407
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AHqMVHJoyUi2JtpjbPVEKOoqsjhh1JmoSte-HPaRuShyoG4mxtugjKntOG-D05YVbV-gI_V9XqZEDsdAHa0sCK1w")
        ("y"
         .
         "AYw4FbG5ouQlVUGabBkEP6Kw3cxLWm43L-6fyyJ9hbrXBGh-fhqBi2EtXARs11ly96LdXJogCsVYLNWf7EesUl7P"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(0 0 63 255 255 255 0 0 0 3 255 255 255 240 0 0 0 63 255 255 255 0 0 0 3 255 255 255 240 0 0 0 63 255 255 255 0 0 0 3 255 255 255 240 0 0 0 63 255 255 255 0 0 0 3 255 255 255 240 0 0 0 63 255 255 255)
      #t
      ())
    #(408
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACkVPPBi-I8wPl1vmqyWi9kBB21ZlOp_gxgzseabZ-np_iDPnFYj4A4LnjWS_KKgMyS133yTGGr_aXrKhkYA1E7M")
        ("y"
         .
         "ACgBpi4vQQbzQQbaI9yT1Q4-l1odR1EAIYNSkGSbekElEJ9la2sLW9ALJNhOobpOHtSeYcUm-xARAFExyu5-4FAe"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(409
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AKYeuZTihyLFmzxgB9_fizeJP2NQ9GGyagDhpFEEMUqumYnah-T6yyxO9yEYW32W2aRaKKECdWUBoazF0ymiG79z")
        ("y"
         .
         "AQ6NDhL1qaQODVnJDOcwQ9OXMK6t03iOMdfCu2KhFmFhmUZkr6ZYzi5goT9F8n-RQwfI1vjU7RarBBuPaZCKYngv"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(1 0 0 0 63 255 255 240 0 0 3 255 255 255 0 0 0 63 255 255 240 0 0 3 255 255 255 0 0 0 63 255 255 240 0 0 3 255 255 255 0 0 0 63 255 255 240 0 0 3 255 255 255 0 0 0 63 255 255 240 0 0 3 255 255 255)
      #t
      ())
    #(410
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AR3Ul7MMc3CZBrFkqaedx_KpjAFI7WMBa7lSQ4NPvN-Ot0sP9lLVT1nzGu9R2m6JdNNjZVsdoTjcTeDyqNgA9HWu")
        ("y"
         .
         "AFe9S4RgdADYY_-_RaPPWJme4kugXpPsp7DkrnYOsXM1WaRdFVedM3DXFv-j7Ev9rkGOMvsGE438ohNyCpOFd2EO"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(1 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(411
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ASg-uT-jaf5wErZH0h4Kl8-ZUOX77YGe9WFY8gyKlHOkGOzLyk3CtH9MttMi-RcAWFm_Ih6ErJgnyrgqgBxif7Hs")
        ("y"
         .
         "AHXEgMuvs1L8r5O68joUBf2B_r4JcpqQjRB34XfdiZPZSyUaDVJlLaPttv34ZOgM1RVA5z0LUQfjQzV23KpOGNtD"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(1 255 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255)
      #t
      ())
    #(412
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ABc77v417oaNSX_2YBYo9lzhihWR9-SjpAZiLz9Qji2mjxAe0C_rw4QYxt38JqXsmEjEJ5JGOx6UX54WfbNL3y1m")
        ("y"
         .
         "AFMHBkerp81g6ylauBomijkD85PF0ou8XgIjUcN3zYTwLBnes2RCNyyuEzLpL5W6YLbIUuDeBxjonSTkPNR5yfsR"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(1 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255)
      #t
      ())
    #(413
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJgpzVQyaHc5q2rhCvjqc9LLU7geuwa1lht7rcFnaz73sARU983lZ3SgExLVdKkZPBpf5TNvvmJiOtm_gRQ3ifn5")
        ("y"
         .
         "ABL5VWl-1XggcZe_mqw4llIWFdusyNxmXU8XFbCEOfScKqbtM3Aj_8zFB1qFlEk2gm25L5GXN8o6_q26GEcIS973"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(1 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 1 0 0)
      #t
      ())
    #(414
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ASbjyVnNQRILuDaTsdagNLOFE3wbsyE7d2Ei_tlgVuMpiFcYpzvuY5wLpLaIGGgvSYzlSWklACvXZSUWQF_MT-yt")
        ("y"
         .
         "AHOpxuOwxpS_fMjMu9CYAOgeNUi6RKDCOBzvCwe_cCoZBUu11xeht5KUYJy9r9TiAYBk97LEwgTYGOt85SHDJozl"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(1 255 192 0 0 31 255 255 248 0 0 3 255 255 255 0 0 0 127 255 255 224 0 0 15 255 255 252 0 0 1 255 255 255 128 0 0 63 255 255 240 0 0 7 255 255 254 0 0 0 255 255 255 192 0 0 31 255 255 248 0 0 4 0 0 1)
      #t
      ())
    #(415
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AVPcSBqzxdyN7NJM6u4b7Hf1nyH38xwZU4rwR9KBrJ4lZ5M_09IQlrGF1AmJGVcZMbubC-cZeZXi-68hyKEAB63g")
        ("y"
         .
         "Aa1p8I_K4WQ5C-gmJWtQ-uR1As4OnKRq8MSQy0AzyIb4hmGpn_K9PJyOfaMPrytMdp7cWDGBCsBQVMl-QQY_SW4f"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(1 255 192 0 127 255 0 1 255 252 0 7 255 240 0 31 255 192 0 127 255 0 1 255 252 0 7 255 240 0 31 255 192 0 127 255 0 1 255 252 0 7 255 240 0 31 255 192 0 127 255 0 1 255 252 0 7 255 240 0 31 255 192 0 127 255)
      #t
      ())
    #(416
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AfWGYRyHFQKIw-hhFsXblKJnGJeIKdcB3awF6bDOIt7ksY6V9gy6eD7TOE2jc96u_Fe4Jl06NO60WL8kudgr4ygZ")
        ("y"
         .
         "AIRW4PHYBJLvAHjMJG0y_Hx_tnILTUWLUbIJjTV0Z1Kw7wNFvQ00Lf7m3S8S7RKzS9ldBYwoEf1HnS3eMhgObJ7y"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(1 255 255 255 0 0 0 1 255 255 255 252 0 0 0 7 255 255 255 240 0 0 0 31 255 255 255 192 0 0 0 127 255 255 255 0 0 0 1 255 255 255 252 0 0 0 7 255 255 255 240 0 0 0 31 255 255 255 192 0 0 0 128 0 0 2)
      #t
      ())
    #(417
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AV7ch_1JmnPqv_0U0ranCo-2m2o50NnE3aIze1PMcuSanj1aLZ6JMM-hGFLawzRDIn-6ZoS9dHMuaHmIS2752umP")
        ("y"
         .
         "AQ7rjS4zYOqXJmKAhSaK8_KgWtQSNdCokgmL1mG2NvfvCoICgpBu2j8f8ZgLmPtZNyKOntzWMy42QSFscwfn8_RS"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(1 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 253)
      #t
      ())
    #(418
      "edge case for shared secret"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ATG0MAL35ofuwez2olPCzMnkjwTYb8zRj-4NLSIZHx6lOcQNUhlwtHCdwDmG9kfg6LszQM-KPmQ6NUEDVDfPJfAV")
        ("y"
         .
         "ALJ6VaxF8ClvjJZWvP1Stc6p9BFcBuTGQxlgmEfUXpJBhADnhoZywNPm5ebgBKcZBHbtd8_DOtGaS9LGFa2ZUPN0"))
      #(("crv" . "P-521")
        ("d"
         .
         "AKK2RCo3-KN1nSy5HfXsp1r2uJ4nuvL2y_lx3uUFj_qdjayAXHvHLzcYSJ1qnLJ4evjJOhfd6xoZIRqyNgTUe3ZG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AD3ewT4NSLOUrihSr1ajt9tSpa__sU7UrYewKGBNDKMvORitbObL_UlQSGXKZHR-p9HVHRZ25XWSBJyF_oXHlOkn")
        ("y"
         .
         "AFitu6d2cyRAt8T6o7IcKkSLQBdppZYdPpVJvSd9LaywzrQRfUJVNprQ8ydOsciIX2KuREMq19F5SVUizFdPXp88"))
      #vu8(1 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 254)
      #t
      ())
    #(419
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "ANIOyf6mtXfBDSbKG7RG9AspnmSLGtUIqtBoiW_uP45hS8YwVNV3K_AaZdQS4LyqjpZdL10zLX85-EbUQK4AH0-H"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(0 83 191 19 127 238 137 34 118 159 141 15 226 121 202 164 218 201 198 5 74 208 70 9 149 88 138 132 93 10 149 158 36 188 15 194 57 26 43 146 247 189 64 15 80 161 26 157 179 127 7 190 247 250 141 173 42 144 63 207 83 74 188 135 54 247)
      #t
      ())
    #(420
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB")
        ("y"
         .
         "ABDlm-k8TyacAmnHnir9ZdauqptwHqzBlPs-4D30eEm_VQ7GNuvuDd1KFvHNlAZgWvOPWEVndw4_Jy1ojIMuhDVk"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(1 201 90 196 23 201 10 82 1 73 178 145 5 205 171 54 245 40 162 62 251 86 33 82 13 189 175 234 149 167 212 52 153 196 200 190 2 205 28 45 224 0 218 24 16 79 168 74 30 158 206 99 134 240 224 239 165 35 74 36 89 93 124 76 150 244)
      #t
      ())
    #(421
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAC")
        ("y"
         .
         "ANklT9-ABJasszeQsQPF7p-sEoMv5UbGMiJbD3_OPaRXSxqHm2I9ci-o_DTV_CqHMarWkamou4tVTJWgUdaqUFrP"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(1 180 126 196 30 58 90 189 157 217 128 143 192 77 144 120 203 237 114 185 235 169 141 60 29 237 112 162 153 56 240 239 213 162 122 113 19 255 114 31 18 44 177 116 17 222 48 122 53 92 104 80 116 245 118 107 109 26 3 61 47 161 136 201 69 182)
      #t
      ())
    #(422
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AF-ID1DslL-sZlj6L84FlFxqNrJmQHtvvVQ3qD4vL5ucUKc0hy5I5w32VFfxPkfQbGuLKfRzWs8QXqY-BRkE0Yrq"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(1 58 239 227 36 87 40 160 140 144 79 231 214 28 217 194 253 172 99 242 156 246 100 216 241 97 190 186 203 147 248 167 16 233 105 47 150 137 72 10 212 152 222 0 240 0 97 228 14 70 231 110 71 84 193 19 14 244 33 122 88 147 62 11 29 198)
      #t
      ())
    #(423
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAA_________wAAAAAAAAD_________AAAAAAAAAP________8AAAAAAAAA_________wAAAAAAAAEAAAAAAAAA")
        ("y"
         .
         "APM__EXaPqwbqrcnq4_TVc-hNMQgR9VSYmUWVPtQ336aWnXxecjIbEOIITtWh9xD3-uzfzAShwPETM1cMoSDO4cX"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(1 104 223 39 45 83 227 22 25 38 22 140 74 234 181 243 85 184 210 166 104 156 253 86 127 43 110 178 1 26 24 199 117 172 42 33 248 221 73 127 105 87 33 112 32 179 177 175 203 112 33 242 79 204 194 82 59 231 106 43 255 68 89 110 90 20)
      #t
      ())
    #(424
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAA_____AAAAA_____AAAAA_____AAAAA_____AAAAA_____AAAAA_____AAAAA_____AAAAA_____AAAAA_____")
        ("y"
         .
         "AM0oOdhXtGmfXI6KAZR4biaoYvCGtLqAdGrlIl7Tqmj5a3quxVIlgwu5j1LXUiEUGJe6SdejHrvwttfTE1LlJmGQ"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(1 61 177 185 36 27 35 211 56 96 211 45 236 55 167 158 69 70 164 26 253 253 217 196 56 208 78 31 139 86 106 200 217 211 245 114 194 147 233 105 67 114 42 78 226 144 225 19 255 250 168 42 97 134 125 156 162 141 52 153 130 53 76 155 37 111)
      #t
      ())
    #(425
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAgT2YKRGfQv-pX-qLqegeTNamypf7B3jhLl9d_jUgHdTMqOyg0uOVVVmXBBOB5qwfGN30x04LbpBBz9yh0cEDCR"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(1 210 187 233 247 84 88 78 187 199 199 173 116 19 109 28 138 20 73 72 148 138 168 190 73 152 157 217 180 197 20 219 46 42 177 224 113 58 209 105 159 99 45 210 206 165 61 162 24 237 84 159 3 10 17 62 40 47 217 227 190 70 45 154 186 132)
      #t
      ())
    #(426
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AQAAAD____AAAAP___8AAAA____wAAAD____AAAAP___8AAAA____wAAAD____AAAAP___8AAAA____wAAAD____")
        ("y"
         .
         "AIeK1ZfSkNss9mBZSu7Q-bfI3WhFHS0bLLyBax7E81Rls5ZK_y7fElUWP1_KWAEy-Fyt4oh6AX580LNxlq2FIhEH"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(0 15 55 162 226 202 239 84 255 244 18 108 15 169 110 124 71 240 202 215 70 38 239 145 229 137 225 45 46 30 140 34 27 231 41 91 233 220 39 18 184 123 176 170 15 88 128 183 56 188 18 66 242 186 119 59 249 235 42 84 227 193 202 71 88 215)
      #t
      ())
    #(427
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af8AAAAAAAAAAAAAAAAAAAAA_____________________wAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "ALXhGRtEn6Hr29Z32qSPkOLR1sBYyHcIfK_ZNk2Z27KDxoQC5ubF9UEbLtQoJNiygM65EKumhHiDp-N4DiEyr0HB"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(1 122 235 37 77 156 140 142 224 98 21 255 51 129 19 87 218 115 191 127 109 214 215 248 241 118 214 44 6 90 136 169 0 95 104 12 99 14 159 39 99 88 94 162 238 118 182 228 171 69 230 115 248 20 235 250 149 148 124 12 99 251 36 250 110 155)
      #t
      ())
    #(428
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af8AAAAAAAAAAP__________AAAAAAAAAAD__________wAAAAAAAAAA__________8AAAAAAAAAAP__________")
        ("y"
         .
         "ACB1E9YVZWocx1BcGKohsI4rHVqEHeCBbMKcAE79stkCrBp7sF4gcitXa2Sj3fTSSGQhrHBr9KQk8lI4Y2ilNA-2"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(0 97 190 212 34 72 163 123 70 37 239 4 196 249 199 239 105 238 60 111 149 3 55 131 81 252 171 27 140 225 52 50 6 153 126 236 27 136 68 158 182 247 53 87 17 234 26 129 138 72 110 227 10 36 18 98 65 167 226 40 146 103 207 93 214 31)
      #t
      ())
    #(429
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af8AAAAA_____wAAAAD_____AAAAAP____8AAAAA_____wAAAAD_____AAAAAP____8AAAAA_____wAAAAD_____")
        ("y"
         .
         "AB_oAMUOVAErdaM-S-fQfI1g8paAo5XpUaajHFCWsOqSj8LL8yfdeE3Ap8pG6nOZK3WLVkE2S0q6Oek3mKTZJaAI"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(0 16 103 217 16 78 41 110 244 43 148 69 135 222 17 177 13 240 93 45 149 158 212 76 172 158 126 241 199 160 93 144 129 156 67 188 121 199 57 121 24 249 87 204 152 219 147 23 99 187 235 27 223 195 88 101 232 163 89 160 19 241 61 96 196 51)
      #t
      ())
    #(430
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af8AAP__AAD__wAA__8AAP__AAD__wAA__8AAP__AAD__wAA__8AAP__AAD__wAA__8AAP__AAD__wAA__8AAQAA")
        ("y"
         .
         "AI3Rih9eSCFAvnm7ZaIa1gyJh-UyyENF8BNa_9Ruxx7wKxyjrVbzAdlV-jBsEi1EHW_tz4uFXvJWNQv2nSOnIHrZ"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(0 183 121 216 48 53 207 123 176 187 4 199 178 244 109 8 246 121 31 13 21 66 201 188 206 114 80 231 114 177 42 216 227 143 206 29 43 6 58 6 240 250 58 27 7 45 217 118 245 248 84 41 121 144 48 117 22 47 31 92 107 163 183 108 196 93)
      #t
      ())
    #(431
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af_AAAAf___4AAAD____AAAAf___4AAAD____AAAAf___4AAAD____AAAAf___4AAAD____AAAAf___4AAAEAAAB")
        ("y"
         .
         "AFZiA90yWggcREHwAfeANlh0_T0Mm8RyJ0ga_napOuG_3mOvlyIDq_4ixjuA6D98whhMPLjP0BUsVDJMR1n9H5pQ"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(1 175 229 210 55 51 114 139 121 199 67 147 59 155 167 223 236 94 209 155 119 55 227 147 144 138 29 0 9 24 170 121 93 28 224 173 83 57 131 208 24 249 39 179 93 42 246 70 51 86 87 63 56 127 235 215 89 17 164 148 134 32 44 166 157 58)
      #t
      ())
    #(432
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af_AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__AAH__")
        ("y"
         .
         "ALEcZo-9VJ82iJ97Y0NAUdom8VcFg5E2sbFKCRUtehgup4BsNUeKMtOqPJwWJ6YVGevscbNvp3RJAluIKeJ_MHg0"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(1 150 18 174 179 134 254 187 26 40 9 111 229 178 246 130 222 173 2 56 151 133 34 91 128 162 125 244 57 81 13 8 52 154 25 56 57 82 95 36 139 127 155 202 191 211 220 141 168 204 23 36 2 34 153 183 181 231 35 153 216 148 100 184 46 68)
      #t
      ())
    #(433
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af___wAAAAH____8AAAAB_____AAAAAf____wAAAAH____8AAAAB_____AAAAAf____wAAAAH____8AAAACAAAAC")
        ("y"
         .
         "AKp178Co2qwdc_MsnFUkFLzPRK-OdDMbR0OefcxJoTWz7mHp9pcX2JtLujVnoZWu2hP77GNL8phLXsa2-A9ZeO1a"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(0 87 6 115 248 122 220 239 73 193 240 17 232 185 241 225 31 127 211 179 201 49 20 208 141 63 81 90 164 168 149 166 199 1 197 35 6 59 220 19 173 29 176 165 79 110 123 71 111 225 13 178 7 4 65 190 252 88 200 207 243 192 142 247 110 89)
      #t
      ())
    #(434
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af_____________________________________________________________________________________9")
        ("y"
         .
         "ABDlm-k8TyacAmnHnir9ZdauqptwHqzBlPs-4D30eEm_VQ7GNuvuDd1KFvHNlAZgWvOPWEVndw4_Jy1ojIMuhDVk"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(0 22 170 242 40 176 174 193 144 212 228 229 184 19 143 249 204 70 215 5 218 27 240 2 144 28 106 180 32 245 147 20 213 182 65 113 43 20 239 62 79 177 37 101 44 71 136 134 118 128 79 181 87 91 116 26 132 8 197 98 91 252 207 244 253 218)
      #t
      ())
    #(435
      "edge cases for ephemeral key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af_____________________________________________________________________________________-")
        ("y"
         .
         "ANklT9-ABJasszeQsQPF7p-sEoMv5UbGMiJbD3_OPaRXSxqHm2I9ci-o_DTV_CqHMarWkamou4tVTJWgUdaqUFrP"))
      #(("crv" . "P-521")
        ("d"
         .
         "ASvBXPOYHqthAsOfmpJaoTB2PQHtbtrxQwbrChTddd_1BAcN73uI2LFlCC9pmS3g_6XukiyzqzmRfahSTKxz8KCc")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX_Hz2ZtrFo4FsY3DNaSxOKGZdWpqnDOUYke5cdvkhRJ6WIFJecYzVJ5zWEu-7-4LxeuEqABQH9V05tpu3xvWbLq")
        ("y"
         .
         "AfYaLq3O-KwbkoMK9io0PLsTJSoszxq3UssVHgvTm1gBdSNL9kHUToZgdz2EIGhBHa838C5Z5UWpRsqcPvB-XAu9"))
      #vu8(0 165 214 223 218 43 38 159 74 184 149 164 28 59 113 182 186 16 213 201 240 217 179 231 48 39 83 69 228 114 21 148 171 253 57 70 76 34 119 22 222 216 239 62 96 187 28 160 181 81 113 110 63 110 235 180 141 92 232 224 171 88 203 27 115 201)
      #t
      ())
    #(436
      "edge case for Jacobian and projective coordinates"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB")
        ("y"
         .
         "ABDlm-k8TyacAmnHnir9ZdauqptwHqzBlPs-4D30eEm_VQ7GNuvuDd1KFvHNlAZgWvOPWEVndw4_Jy1ojIMuhDVk"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 143 97 225 94 140 133 69 220 218 177 136 241 11 169 17 27 99 69 213 41 217 197 71 6 119 52 45 247 239 84 197 106 31 185 251 232 222 167 106 251 232 242 221 76 60 251 77 91 116 157 116 57 68 201 109 116 251 71 188 75 246 1 229 220 126)
      #t
      ())
    #(437
      "edge case for Jacobian and projective coordinates"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AE3BbK_KmDPuuXwTbBVPOuOQgw8m0wDt7wb4Z--rHEIU9WPCXhyB4WqG6qyCcoktG2Wy7n-ytpuhEQsIO762uIc6")
        ("y"
         .
         "AQ27cBJmqN8y0XvVi-o2XCY31oYnKQCl6noZ_5jbO_kkJaSDxw_dnbJbbuaZgctp3MnEGMMpiec_Cl_X88O6RLBR"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(1 126 126 236 219 15 82 227 184 174 221 15 85 80 242 108 213 226 126 113 29 104 96 197 79 136 207 211 255 7 93 248 211 99 238 59 228 218 194 244 45 3 107 124 100 226 181 13 144 118 74 180 238 240 185 214 140 41 104 43 151 7 212 94 194 131)
      #t
      ())
    #(438
      "edge case for Jacobian and projective coordinates"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AYJVwBTyUzrZMOQyAhZJaj9ODXi1D6J9QiCeJw45uvSAqYeiyECY7uU4ieqLbMkDbd7r78ALVULSQl-v1eG6uuhB")
        ("y"
         .
         "AduBezMrKXoAPP_EJRtY2cfOC5AwHvXGXorA-CUX_RcwoWfYOvUNkvfiXgh4cTBhiSA0XENTN-9F6y6bGs5TDQ6v"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(1 21 186 127 193 239 93 21 71 187 37 157 115 2 207 132 64 1 96 165 153 163 253 29 54 142 77 143 19 104 72 188 50 179 69 167 146 107 149 20 12 49 157 179 157 61 137 71 157 68 174 172 192 92 138 243 125 85 69 12 26 225 20 190 181 131)
      #t
      ())
    #(439
      "edge case for Jacobian and projective coordinates"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af_____________________________________________________________________________________-")
        ("y"
         .
         "ANklT9-ABJasszeQsQPF7p-sEoMv5UbGMiJbD3_OPaRXSxqHm2I9ci-o_DTV_CqHMarWkamou4tVTJWgUdaqUFrP"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 214 57 101 193 80 220 198 200 52 221 42 148 70 169 0 110 1 26 255 184 36 26 56 227 64 151 82 183 244 120 211 44 109 70 31 151 130 150 165 115 57 255 74 177 163 8 248 253 3 48 169 50 151 155 63 194 54 61 4 83 142 114 81 1 118)
      #t
      ())
    #(440
      "edge case for Jacobian and projective coordinates"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AXPemQl42lOgXEY6VnMEcnu8nb7Y1dqzutEdGATeqGT77Vu-yAfBPkEodJz4wRcnpMUo-R_wIX-VOjBI3lunormt")
        ("y"
         .
         "APAhPQMsxnThApxx0Qyz-R3lN1OgHW3cQQZANqQ9YT8ruD-5mYdKD63Pim1AuRcTzJEUvUTA8TM--Y8O-2NyqaRT"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 129 90 97 242 199 98 45 27 158 51 16 16 74 65 197 118 131 90 41 207 58 135 94 59 16 98 228 88 135 22 210 87 119 240 169 15 166 169 154 149 34 99 32 186 34 91 137 101 208 239 90 246 116 251 166 154 138 203 133 11 119 115 190 12 130)
      #t
      ())
    #(441
      "edge case for Jacobian and projective coordinates"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AQ9DZi9LrWGHrSKkHfyDHcuiVa9sS18cFLLtVEf4i2XWkLh1hI6noMTv5VuCFIjRsPhc2m5xc9h-DXZEGq9g6WDR")
        ("y"
         .
         "ABf6S1EJfF75y2bWw-uFHhqKQRAkUr0_iQLxfucqsHciQVEBRGdGhkGcfNVakwlRFlugfTTCwgxCEGnB_j2XZzek"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 16 234 138 247 101 103 53 23 102 215 221 203 115 67 238 4 237 217 80 53 247 39 230 186 224 216 157 80 23 1 154 61 247 149 84 209 208 232 79 227 57 167 192 253 121 130 156 218 225 55 46 93 153 0 160 220 139 172 99 174 51 6 109 63 17)
      #t
      ())
    #(442
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ASGD__V3fjGejNI6tTkrqp1LHUPH-LAfePKVwMN9SjoSL4jBY6j5ZI2dojs4m6VujpygIsL-nCc_JYVCmi3kMpW5")
        ("y"
         .
         "AeGOnqr4pAbxCx3LqmhHQ6Q9ID9s3dhwnbkPQ_57izgV6tIEa2s3I54GXaYteXzQpzEi7pvxvQCw2AEpej0rzv0N"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(1 243 201 180 79 175 117 75 226 80 194 237 117 101 65 194 97 203 83 121 90 85 94 146 71 130 220 79 44 29 212 163 133 92 130 63 51 189 252 176 240 80 62 163 249 169 17 226 66 53 55 90 105 218 33 169 174 76 100 119 56 183 229 197 41 9)
      #t
      ())
    #(443
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJYGilEuMX7GNYj_KNOAqNfJHjpZJ5yF9UFujtMhdHsF1TfRnXlzZK_ODlSL-3WOM8xtdRtcIXly3i7E_wAMwV3u")
        ("y"
         .
         "AIWv8PAlKrRv-A2EnhSUPegUXbswe0vEXJ7t54wECgg22AQGdwx8lFnAZcUzZ-aDEo6Pwd2JGQ50eJPZ8n5JYQv7"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(1 245 103 208 17 82 201 6 68 190 52 252 127 77 205 172 218 2 225 225 77 128 205 179 119 97 26 140 154 122 77 15 165 65 59 157 65 84 16 201 172 111 241 70 124 243 114 53 229 136 209 20 165 78 111 158 10 101 182 208 44 219 130 136 152 20)
      #t
      ())
    #(444
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Ab_Yw-NG74hLSxwB_Sc-mC_tOLXIBGFlzB4N6HZ4dpDBnE7CRga8jh7YOkzhC3pC5tsZx6pemXFlTleELUHnCYWj")
        ("y"
         .
         "ALNXNRSZYAVguvqiXiQ6_3szYCz0HTUYSZ4bQ7oegUoLRewB_mlHieAVejwoHRf6AOcBn0euehDOQO0Mt8pWtB4m"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(1 129 156 211 163 179 104 131 196 128 194 112 219 19 154 148 215 249 74 253 136 121 179 78 246 90 48 77 11 154 98 1 161 219 207 203 142 229 196 230 102 152 217 141 208 232 189 213 62 86 54 40 177 90 254 5 244 130 48 250 28 82 149 45 137 137)
      #t
      ())
    #(445
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAzIyE0dtx-5FZXwBwPfJX8qAwV4R7S5UG07JhWotyQGLZOmKVQhnfnsmGJh-3cI6qE5VBguEF6Ok8ERwNKovcUd")
        ("y"
         .
         "AOIMxkiMd2HzGj52JphVPl8r9JtBo6laxbSCPCl1nHOBUcMBnOnT2e50EaQQbVgQyLiq_Tf1RPHK839vX61ZLlm9"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 128 133 156 7 202 225 145 27 117 57 100 238 23 92 136 132 189 25 171 173 102 110 46 71 46 214 50 158 113 249 81 83 66 39 178 116 40 5 198 230 49 15 110 163 35 11 212 3 194 38 14 151 176 215 241 39 64 39 247 230 159 199 184 18 115)
      #t
      ())
    #(446
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AFSUAjcJ7mrjnQxbZ9lZ9fjL0b6pZEKTOokp4zK3BPcUbsxom3_bhcg_OmCEbmkt3gwnSM177LsNa0x8DA95Pzre")
        ("y"
         .
         "Aad9dcJgBockGEPi62rIGrGdD4qXR5iMA_LHcFk9-kgEjygW44VsXxHOPNzKOuDh8TcYwsXbLByFn3yu___f1in8"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 157 83 42 248 217 157 252 12 233 62 43 99 48 91 31 5 92 66 175 255 63 169 1 17 176 113 52 28 202 212 91 166 58 217 237 58 20 30 149 192 205 13 112 231 138 126 189 130 194 46 104 250 196 106 165 201 51 90 149 86 49 249 183 198 106)
      #t
      ())
    #(447
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJEoCbc_UEdaO5fpYCt1frLpT9JF5IMEDhZ7gzhApi3yeRKpwuAxfb_ljcQ7ogU97t5eyisivQZ5IAHbyqfqa3AA")
        ("y"
         .
         "AEOOW7hPPveGUVKyMX2N8MaRw9K-0kZ9XnUHJop7qYyrTkYIzqD1-_UP0KSH0AWzgZBclar2oYzRotvWdCtbBbpx"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 27 1 143 236 242 192 120 17 165 75 67 160 178 132 201 150 238 204 119 66 33 2 105 55 60 57 219 162 153 209 171 145 199 120 176 223 64 170 165 37 48 118 102 80 255 17 120 222 5 182 156 59 196 107 41 209 175 25 52 51 250 18 94 185 0)
      #t
      ())
    #(448
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AEPP4sMNP58P-W4ON_Yest6jgWmZyRxbd5V--SsOzeCSx1BLpXQG6t10Q4GQYzfNE65FVeUKnrJXFowhQOgopGrT")
        ("y"
         .
         "AAXBKfqXJagtPm2nIewNFBL_bWotj3W_x7K2qtxfrYb8GopnzX5reqtCpUYXnG4FYFVNtfYTin7y3HnSvjhTVsnq"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 20 80 216 64 47 38 157 251 99 100 153 241 100 84 69 16 108 61 149 249 48 128 201 148 114 109 183 228 63 182 132 136 247 67 15 109 23 21 67 161 39 3 140 147 2 100 230 13 191 15 72 211 50 175 33 14 205 50 50 11 28 185 10 207 96)
      #t
      ())
    #(449
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AS7akCDGZa5ppQasazKHRl4PwBN7IQdc-2xplj2ayzmibKlAw6OeDZTAVGcv_Bdh5W3tPBgPAGhX0THcNKWs5_1Q")
        ("y"
         .
         "AXCeiizZnh4gFsJPHFSF48R5R7zG7769IhHA1SmoPQl6xhGqUNeHmXn0vi2c7MFjYP5LlJ6plw8Qdcv5V_AxzP2y"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 120 70 162 11 67 164 152 39 121 4 104 111 55 102 185 163 173 153 77 198 19 41 219 58 233 217 118 24 184 19 12 43 190 206 13 167 226 199 157 189 14 29 200 48 58 229 249 144 61 151 21 125 10 116 219 127 70 90 190 153 101 207 168 62 238)
      #t
      ())
    #(450
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAhLocuPM8-6idHBTtK1Dac3oFrgpMo1_t47JjJNO-dPXjMgqQvziLz4u8xMPByI7VIQPMfFyitZzNPn36ttL0vc")
        ("y"
         .
         "AaPxdcPIqJ1P2pXUey1PMEeuvHXKfqQW7XbY5onX6-fpd3N7xu_5cz5qbAWTSG5ig0_0YSG3L95bM1n6cH-6bWfM"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(1 142 100 95 190 221 159 18 212 71 77 177 32 106 166 112 87 224 141 245 103 193 238 203 218 57 94 219 32 73 247 112 144 142 53 21 204 231 121 227 113 105 235 192 38 235 92 53 48 64 5 141 168 95 186 189 103 76 141 82 167 138 57 114 49 150)
      #t
      ())
    #(451
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aeo2zk5Ru7Mz6pQqUyXhQQiW5zZSwoM7RiJi2l15Gxg1U4sGzZ17l0HzQU1SDah_i-C-IYBDsFytYbk7C8BITgso")
        ("y"
         .
         "AezijfTZyj_7I9acNpZswmtJL3AQ36yme0ve1xz3TCsKhuYbx3kDOX64pDYjefCUkqpj2cn1_zDRBbUwwB9GzXzc"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(1 145 42 161 119 234 143 136 120 68 123 74 25 218 23 212 130 53 122 39 45 234 25 181 66 57 138 107 180 139 128 89 135 84 174 39 199 57 88 4 106 170 28 105 149 19 2 114 125 221 170 249 102 240 63 245 183 35 172 183 219 127 204 243 166 228)
      #t
      ())
    #(452
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJyWAk-sd-ZLhG8UHNEBHyrVLjqlURA7luNbQ5z4nAMEKT8b-1IvmB-7VpIduHFR55fQfxEnoBw_PeVt1JllbBA7")
        ("y"
         .
         "AXe2f7Jni3INCO0FpcBrz4gTQgHBkpl_xtwV-CC2C8wPd30GYb2Pgc_tWd7sM81pbhwe9yx2Zs1a7UnrMlcUc5vt"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 106 91 12 9 167 189 239 49 109 37 243 149 121 39 158 40 107 120 28 46 67 15 32 28 84 207 201 225 84 163 142 195 30 238 157 9 246 213 174 212 80 86 197 183 187 68 33 60 99 87 199 73 45 31 217 177 199 219 161 22 250 81 210 28 250)
      #t
      ())
    #(453
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AI1NZ3qq7zklodQfzk0wVDofozorMFGzZ8_fG42nzRq2fOm7JV5gR1aERDzxnqLh8B5Y_Xn0dyVmG-IIdnyyoXUw")
        ("y"
         .
         "AQvhdeJNO78n3K0llrgJDH8uAFkBA4ZXKEqA_3fTiUop7i19qliAs7oFImHkeW8TLbnoqXgfrLMr2KCbq9ngy6kY"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 101 47 186 164 17 173 49 109 234 64 163 125 55 152 208 192 60 167 202 89 13 142 234 238 69 76 125 36 255 28 78 199 223 253 94 243 233 7 98 213 83 156 90 87 231 69 185 192 248 142 24 255 246 247 55 114 158 103 96 109 223 227 167 117 25)
      #t
      ())
    #(454
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJt2v6OWs3BjgVUq0I-zufkrSSxD1hArfALMpAF3GOBkNHjA1zdlwnlYE4x_bSO06Duq8n6vIX7VZeZgK00IAgCx")
        ("y"
         .
         "AXunrvxFZ3uMTUykaOip9aL8QRpK9P9Fk6m3gfE1eq5GmIZCoSVNbPWIcI7-YLLfqxXTcbpPTZ4J5h8G1NJD5exX"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 177 174 5 194 169 31 126 30 157 98 107 26 70 190 210 0 157 214 102 46 113 36 22 225 156 27 233 229 229 18 236 115 65 39 173 175 230 18 154 131 215 75 167 167 151 244 26 252 75 17 69 161 111 110 56 145 105 200 182 89 43 246 40 247 71)
      #t
      ())
    #(455
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AQCJGQ6bY_WinkG1oYN9m_QcCyyhB9bQiPTB1Gh3MVDX0UXM5wokCCcmg3EUTkrN2nnQmlGzHKILrBGX5hnRqE9N")
        ("y"
         .
         "ASLGOMBpElhlgMen3uQpsboAD2KFt_2kQTiMMjwIlf-QRD8GFQEcqjMmIuZ-3oZ6jERTNUROoRLoBTLV9t8UCOcr"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 3 125 62 67 89 195 38 247 74 140 220 196 187 130 9 14 147 189 72 117 123 108 175 116 159 179 154 131 249 146 167 250 150 118 173 70 108 141 113 147 144 34 20 218 188 252 16 75 200 102 78 185 52 162 223 100 139 46 31 64 29 116 94 22 179)
      #t
      ())
    #(456
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AHBVBTfjheRgFJCsWng0jvJmeKSrOnRp8pncSTvSl2QW9knuPjzddxkdj4-dYA6Muf3jr-Y14iRjW3D6eesQU3RP")
        ("y"
         .
         "AApUaVmuSrzagnQppHqMvGr-QvoPg5EpPad4vilsci4Cx_9V4gwRlTHSERc_AusQj-G4uxDUZccVjdPLLZ8yDRcR"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(1 142 34 166 61 254 177 79 40 212 243 56 183 132 85 135 120 81 233 91 52 151 150 36 94 255 168 48 226 158 161 151 132 153 240 122 217 112 75 53 39 70 218 195 11 251 184 39 225 240 11 151 159 100 43 184 134 241 86 181 56 172 107 233 23 70)
      #t
      ())
    #(457
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAG2aViTHvMMVPgxlcxAUwwORLqiI9KNi_qr8Q0WMC86GWAft078vvwbcdHoGuT2Cz-NfCHeHfr9XPGclLOWQQw5")
        ("y"
         .
         "AI8TxC_0bRoY89l1_bwLu0PDHalCP6k19MIRQ1UYoBQ3v662qvAx-SsMN9dikP9oI3jCZRzZderBks1lgtu-wJl0"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 50 40 78 211 225 93 94 157 237 165 60 234 245 175 242 28 35 14 96 56 230 215 229 202 174 42 113 90 82 136 188 184 70 156 226 190 250 175 59 65 132 234 179 72 105 20 64 149 102 23 136 211 253 244 26 198 30 241 37 104 144 220 122 11 235)
      #t
      ())
    #(458
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AFBXEWfcnzsfSuMB3R-sRwAtkBOYgj8uPepDOCiN6Kj7QypoO9Ur-jgHALJ6kErWClpNNjlR_-dFQNVN6OfUjqFs")
        ("y"
         .
         "AIX2p6dDlprifpqWvGA3A5qH2Xxpau2Zo59uLVas5VgHTUlUbkF9eRql_wScnTDDBQL0ACCuQeelEmcc3MSbYHm1"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(1 223 180 106 254 11 240 100 223 240 180 23 127 127 210 127 162 238 8 50 175 124 220 146 31 102 192 229 190 48 200 6 48 196 30 9 32 46 162 243 112 245 187 208 46 210 152 176 136 32 206 134 226 203 114 74 25 98 198 231 177 6 47 80 50 221)
      #t
      ())
    #(459
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AX5PQCsc4gn2c931Cuh7nvYdgU1R4U9D2jwj9A-sQi8QWP2JMN-vEXEMQaf2t5JV4cLPutadJXoOzBAvXjjhQH-d")
        ("y"
         .
         "AOEKZ9F1s5m_GUG9D8ExJ_frES4ahoGsws-uDcKVnoI3SIFGKByj3yseYFbuky_rGsbm6d8_em7mAhV4oP_bUOyi"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 23 185 159 171 14 224 215 249 63 115 9 255 220 145 73 186 60 182 131 4 47 179 208 43 213 49 158 180 38 120 172 122 199 222 33 78 92 15 132 115 98 126 241 81 18 192 209 163 25 66 132 182 252 204 245 97 208 210 82 173 37 252 201 57 83)
      #t
      ())
    #(460
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AATNl-EtBit0kL5F2F6FvUAcF5H7tE50pFQ4qDF8fsnwJbNMWVtHacezf0Qpa0uPc9VkHaKB_jVQiiOtUD2pHufb")
        ("y"
         .
         "AXxO0NhLQB2rdIjag5sgtXgJ_nEn-mbvlNwCnkSyUIV7MJoRyR9zanbzyJ2a3tl_WwvuyTp7bXiC5Ci-_MINkN8V"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(1 182 106 6 94 98 202 14 164 156 37 254 42 55 121 155 70 134 126 39 79 139 133 58 83 77 214 205 26 216 28 193 61 39 242 121 209 238 91 194 217 109 11 119 1 171 178 210 19 250 55 131 107 188 210 209 216 147 123 109 32 220 179 91 83 224)
      #t
      ())
    #(461
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AbI8YO79veB86XUdn5k5v3VFjm8q8K-j2Q-hdi0Bptbj-kCCw3rWvwO6j_F5Gei5YltakJySXZ_rfD7hmgQlOFzq")
        ("y"
         .
         "AA32hAbGdL5eH0LRS_UMGUYK7r3eN5rTEwozLo--6hVp0xTVB7N_Hc0oOxwXFYUr05uBxO3K4PLhzlNYxlmLjJcK"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 89 163 32 80 27 222 107 155 38 130 54 249 102 172 136 29 237 233 140 230 53 106 122 144 10 69 44 89 203 248 216 210 6 48 50 22 37 198 137 199 157 255 255 174 127 135 10 127 191 41 134 149 137 108 172 221 45 14 19 183 2 242 46 126 67)
      #t
      ())
    #(462
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AGg2FUR3252wO7ikBMWULTLhzeogNU7alpPDMzkUtRkMnWOgQzpDBiuIYNXlnA8nrGMwOEkdRrkfwR7O-DJudbUO")
        ("y"
         .
         "AUF44bvwhHPuW6hWhWQW5jJsr-b3Qid2joNcJcUguuBdHUUfY5FY3Mqik2QqVyrHYS1g6W30xnOrjypO2C5eKvML"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 32 76 204 83 86 33 155 238 243 182 217 14 162 239 55 133 247 111 20 170 59 132 202 4 246 178 165 233 7 0 89 100 49 84 110 16 78 119 136 48 113 71 49 44 168 144 152 79 118 181 60 72 157 224 76 199 40 239 0 61 36 111 221 179 96)
      #t
      ())
    #(463
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AZwm2BW76SNY4ezQIUWl9F4fj0bLkLrhMe5YnlG7CT2EECBlhcA-18Wzl1RFugf7fdZoqOWMJ-Ad0AsI_kCbREDn")
        ("y"
         .
         "AJ_oWenSK7FlLjhNWuV55EoPjFyC5ZtGOdQqmyfLFAWZwSNukyM43OA95Gpg--zmH9NLjs59qTe1zB4-fOvM4znu"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(1 122 130 64 63 120 248 165 23 41 232 188 99 197 222 155 103 197 114 212 31 42 172 40 189 217 87 146 230 88 51 143 25 189 220 69 166 161 203 27 178 117 248 169 16 250 100 218 174 37 2 96 10 169 123 113 244 155 40 152 27 113 143 106 224 102)
      #t
      ())
    #(464
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Ad0q6VdkB4SWgEUppe7Q2mGCvg8IXJV4miY0qVafAICGGxg879GhSJ0PkEZTU7lyyEUMKutf2AvmTxWnGdCHPgoW")
        ("y"
         .
         "AOEJjdSzD3jj_mwarwBIsVsdD5j_B0TByIhkHResIWMpN2vnET6TIcp_n34ZNd_2Ybzp_76GrtvJM_JDj8SCxGK_"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 136 121 125 49 249 182 111 174 233 126 235 154 105 92 246 104 199 150 15 73 189 206 5 27 47 194 115 213 234 109 134 199 134 136 238 62 244 190 20 234 176 101 156 87 204 236 178 189 177 144 229 11 224 79 101 7 153 80 197 235 144 214 118 238 87)
      #t
      ())
    #(465
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AHEk7Z0mG8Pl0plH_0nMRCDbkNOvrH62q5G7qwkL2E_8w-8z6WO8wFaFkQ7mOwY7ua2uf86kEZlhmYjTTWR30R6p")
        ("y"
         .
         "ADlzjtVSnzGlV5cPiebJJ_4L3WQT3DEjfnUnD-6OO6bzok7lZRbs4RdzPDhai7dZ5X7nlTPzyFcye3VCZ32DS437"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(0 43 162 93 1 105 137 234 36 213 95 205 217 218 230 155 199 103 164 243 40 25 53 245 6 98 3 129 41 119 115 123 106 37 106 232 57 205 233 178 115 40 22 114 215 216 93 251 64 202 137 95 193 68 156 228 40 63 116 22 205 145 58 86 216 224)
      #t
      ())
    #(466
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AEIgsWQttjgQ-Wz1T5O6bDybFrqoq0LsrMKBovyjaL6HYCS2rhioqvnkM3mftDwPoXXUykpeEhc5xySemib8kETX")
        ("y"
         .
         "AbBq5Y3D97dzAT4tyADgASILrS5oEVymdRbpyBdj0X6t-tLG04Im9mYjotXBF4bgf6zkLXsKHfltCVu77fzTSOTW"))
      #(("crv" . "P-521")
        ("d"
         .
         "AY89vjcTXNjIwEGClS9ub5v9u2r0yY8xujCrxo2I4eqYDU7c21sZ8GEAghlBN-uwGeemZLUiGJoYbL5aAzdsBxP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOYvoeGCFR5d1Sk7Dl9m5nyv0TVgAsRiBlLu6sU4CMudkH5McP4M6QkOzdaUJWDw8I6fD-s-_LHnJRQdrhZKGkUK")
        ("y"
         .
         "APoeqC5jIJcjGAr8frNZbwZM3tZ4r72rfcN8vqAEgdjXxxmR5htufSaevTjg7-KlMRGbEixh2bmPhgG3qhDnXG8i"))
      #vu8(1 245 100 110 114 79 99 126 29 78 168 153 124 198 114 104 218 63 116 27 154 246 210 31 48 119 90 154 155 9 119 237 117 191 160 202 90 128 219 45 68 130 45 228 148 209 92 135 211 146 207 244 172 236 251 80 96 155 67 215 132 89 40 40 101 65)
      #t
      ())
    #(467
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AG2z29GvJJmA7_1iTp9QDk2fpEbJvpTjm0hgdntVsTEIieF4yvbIWfYP-w2HYSxXkIM8rRYKWyP3QSIaEj428O63")
        ("y"
         .
         "ACzoOfK6MHKRkpfuWmgpuHRq85-86fpw279ZvSCbtQDWEpQZupYCCiVQvgoZQm7p4RHmmHrwwybhrh2kNl0Vp7v1"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 119 112 23 73 192 197 75 214 213 74 218 57 170 34 34 229 180 211 34 115 234 22 89 65 209 169 166 17 77 44 190 96 158 230 44 35 6 188 92 26 186 190 8 34 114 21 127 218 200 221 163 156 123 69 40 32 201 67 219 66 103 254 216 194 235)
      #t
      ())
    #(468
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AA0vyrYrRn-XigJn_zWY5v3_CH0yN_j7AMF9D1rMu07e08TEhY1FUwM-WZRgMMb1Ijd5ohIFuipJtFRb5Za3b5Gy")
        ("y"
         .
         "AFfJddoFIOHcqlGJUdsFeGSWbIGOTmT6kQoMDH-biUwGRh_tm0T37EO3DjM52g9WaqwH_eUBu_IocUUtulyT5dYj"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 245 36 177 114 110 136 157 20 213 253 112 187 252 118 251 46 179 45 194 66 95 17 42 95 202 82 3 48 84 22 184 86 9 243 55 207 135 239 135 142 100 227 117 250 41 235 50 249 241 57 191 119 92 13 50 41 167 44 46 136 9 121 248 34 191)
      #t
      ())
    #(469
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACzKHzfdlKCkfIForOpHP7BXZScTAV5YV0PJ0z9bfQHWOLwNA5FHWBpfMJYTnl7o7Ti9y3LN5JOSR3ZXm8uyUilh")
        ("y"
         .
         "AVvUhZwgFcCBaWqxBbiSLB-BzkphBOW1azzZnMw8UGbLwzmuaFqQTfXdFUhcw8py0shPFfYlOX9GBZBbNhcB4A0r"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 202 32 213 86 191 27 175 16 21 62 44 29 153 181 143 168 216 242 28 215 238 243 10 252 148 239 180 125 56 187 54 74 189 181 154 50 109 255 196 31 192 86 239 183 20 56 75 93 63 9 39 11 72 6 252 176 226 113 56 60 218 236 2 59 175)
      #t
      ())
    #(470
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AbU4b8YVlzB2MrPyyYT-DujiLF_ZBLHRSzVo6et2UAN52RfyQwt_L1XgusNW50Zyoj3ypd3Ub8Clj59UBwk3JsJd")
        ("y"
         .
         "AJy_mDMD_L4LWpJuKMBlTtRXQ35AEMzB0eAuKGiMnyEMdq8C7Q32tyf7GLk4cY6dzCwkIaY51zKmFxGlpF-qG6FE"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 32 109 155 199 71 240 231 89 29 190 122 136 149 10 120 219 39 112 206 186 196 233 31 113 179 5 42 51 107 209 159 15 144 107 114 152 235 146 121 11 231 189 163 60 40 33 107 40 22 177 0 254 49 186 138 116 137 169 236 163 186 130 185 209 152)
      #t
      ())
    #(471
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ABLZ69P-tUO0PPOPe6upwg_NX-Le6QScqIVBhKLq1kqlQF4WmugnQTcllYMHk57iRAwhuNpYs_9RauVplTz0hzfu")
        ("y"
         .
         "AM1b7Gz3KGMtMeOhmaKYJA3e0KtdKzUTvLLRFMa48nmgdf8MVqPunkBYrOyJNr-GuyhEuhMHpDT7Q9KJdGZiqD3V"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 40 221 225 52 28 161 123 55 239 179 75 48 238 71 155 118 62 135 141 200 0 108 140 238 67 92 11 210 16 115 197 187 61 245 40 101 189 65 49 245 206 196 21 136 239 119 142 241 74 244 202 71 220 73 161 109 91 112 8 237 224 182 230 177 182)
      #t
      ())
    #(472
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "APOqSf6vaG_P_joKNgaS7wL-sU3m4P2NySKFeynWg3pZzkATvumb-H-DI8Orf1fgLnsKDfqgcr8a2kg2vkkI3a5J")
        ("y"
         .
         "AGr4GrPjBfvlzoqBn4FoqLROQLFuroJopg6l1mwPKENl5gmhkSM3C-7kINDwGaGYQVbOKa3klRDnoxY3BTM7hYkU"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 88 189 77 32 109 241 207 179 254 52 156 218 39 176 19 213 157 214 245 7 192 102 192 182 207 69 138 165 74 28 19 131 37 84 198 111 242 127 97 117 241 191 64 88 55 140 159 124 122 157 139 129 14 64 163 168 175 25 254 14 72 199 40 78 211)
      #t
      ())
    #(473
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AFRoLBhvm4DEx21wUPahgPrZ_IUZQZV0v6_KlnjFnKleIlM0jgsVeUlGHpVRyn3fufro_X_SOohV2ZYpE8ZvL85U")
        ("y"
         .
         "APQFKXMQ7UEjnFZcBEz8TK0oaBJtREs3PVLgUYJFYAqZxA8b-lzKfR85fBjq0zxNP9TUMe-E8RKSOOKW4-174Pme"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 97 46 186 203 108 49 175 32 8 170 38 76 69 209 206 231 170 83 61 120 225 252 244 128 163 17 141 35 65 142 35 98 31 12 233 199 80 159 113 130 191 35 160 42 101 48 101 155 166 44 95 22 248 99 74 79 116 212 195 43 38 198 238 204 39)
      #t
      ())
    #(474
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aeq_MZJT-INBegziZdMUI5anX4MPM1iUhoibvU4gkbgn_vYJCrZXaKOP-zqcl3lkZGNNBwYirbw-mvGY77nXKdfK")
        ("y"
         .
         "AAL6MFgaeyaB9-Ik6tE17HphY69PQKozMEOiut7OrBNTq1_77vkpYJE3zUN-iszAsCQRYDlZ1hx6cPE5GnqZIyen"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 106 50 53 223 107 133 52 62 171 130 53 6 32 136 159 235 162 115 96 78 89 235 152 215 143 230 59 212 65 206 57 65 30 173 132 2 226 80 127 225 10 3 250 217 86 44 59 228 72 131 120 207 42 31 76 66 120 94 125 147 33 141 153 254 142)
      #t
      ())
    #(475
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AdNuaWqtu8QJtHM_w8htyy6YCht9AVj4KiflsTN_0icuUqJ_5nzFhytbRgQNiDJbK1JKLVuHO8VP8FHB0WyZ2byB")
        ("y"
         .
         "AD4_axpzfaFoAkMPgJ4ITNizQgAZwvnxuXihEo_8uQC5xPrxd6eHfkqDhHKKHDCdK8-0EvqYOYKua0Wf4H65x57d"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 9 202 146 154 58 217 82 93 10 225 157 54 180 13 197 67 210 52 143 139 13 149 197 156 66 7 248 106 137 136 73 133 142 147 155 68 88 181 104 245 124 5 78 233 217 86 50 170 36 197 213 241 212 234 168 159 175 143 172 77 17 218 40 221 233)
      #t
      ())
    #(476
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AQYaqJoxfGO4JPfr_IvZU04pffm6z4C4NQy9lM3E09gSeoDblVW3fVDqgsmeZn9v5Tead8kHi_nHzASavp6sfxdM")
        ("y"
         .
         "AKmr_CqJLxBOMMsgzCv7prlIxmN6wGLoOuR48JaGnxfcG7Tq9S99Y1GAMe3ssiDsFLBlLmyW4CNJDV7Qj7Qlnan6"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 57 80 221 182 185 21 69 65 37 227 122 3 132 87 148 254 50 3 126 221 192 65 67 217 28 191 42 184 66 73 80 18 137 100 96 211 130 67 116 178 20 236 225 15 57 210 172 96 107 180 178 249 178 117 72 226 23 32 174 19 38 109 20 190 176)
      #t
      ())
    #(477
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AN9Y8ZO21P26HWGk4HOjBcldukyg_dWPhu4yqTBKYgl6w8DR-uMs8uTmW6TvxtBAhyX1cnT5vCKJ7UJuwnB5rgtu")
        ("y"
         .
         "AfB9mOKrqNOObVSxSW1oJRAEVH9QeJdUxAl2YoJ7yD8s9ZGS654JARswxUytn5JBwJJBWjZv_ltr20z8vNxG7cwS"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 161 188 43 94 142 10 98 16 239 138 81 77 237 153 238 227 10 200 233 170 119 88 173 236 196 145 3 1 252 98 106 86 102 228 84 28 234 3 169 15 30 89 253 130 184 205 135 8 111 38 16 82 18 76 212 59 28 137 253 4 141 173 95 41 17)
      #t
      ())
    #(478
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AHO5F6NWwN6P7xRZP2bV2rHw1WsEK6ZdID7BOcbLcdkLt9wERNDjsBvtl6XGnydUnJLpW7x-jgzpMubEOnVVDobs")
        ("y"
         .
         "AJ8gFOKayyslW_bskRi-vnMl4fEf94hdGi4UKUXLdB4mtP_G3nK3FaFIuEDfVJtJyaBfiCfz7wXbmKT2ZPbdhHjw"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 243 79 59 138 226 240 254 195 104 57 102 51 245 129 185 60 56 227 41 119 0 133 165 26 147 69 112 135 248 17 99 134 213 141 234 84 185 156 229 106 128 137 106 173 64 194 223 230 155 45 254 18 107 225 123 241 90 183 241 156 230 103 191 244 247)
      #t
      ())
    #(479
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Adhgq7XGX20Xkbw_1ibj2hzokFFEF9Z7QHFo3H_JZkH9pW-xgw82lasBcdeGZzB0ez3OPeYc5w-DGvheXG6uC3x3")
        ("y"
         .
         "AbHrqStlun7paR2QihStChFqTgm0DJ6kGX5gwLVPlIQiuql9gLTMr3s1vmgd7Qrs3YvdMFkFSvg4BXiISJwAo7dZ"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 32 61 246 13 179 151 50 32 123 216 219 135 80 27 205 132 129 102 74 145 17 200 198 91 70 240 2 176 184 49 89 150 14 71 16 235 165 72 240 3 157 178 79 102 25 12 86 5 51 112 179 201 136 77 68 89 113 161 41 109 164 214 86 249 195)
      #t
      ())
    #(480
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AbNBp7WBIscLcucLBRozUYbIn-R7j2jDXQmbd1n7YghxiHXYzYxF0RUaNwIBobBHAC9hmwXK_jnUlLammlK0JlMz")
        ("y"
         .
         "ASfnC4_IQIkTla_TsDcy0KUvR3dlHD2KhXnWZYUQ2Gi2nxQ8fuzSvkEuCtKy167wcXeW8_hID6kksJ35AYjevlG7"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 178 45 148 181 63 176 13 72 225 47 18 86 14 175 205 236 126 161 87 201 176 66 67 111 198 102 129 222 150 175 152 186 152 173 183 250 247 157 18 179 153 233 1 236 123 201 213 17 120 131 218 32 90 194 107 56 247 136 167 136 93 67 75 110 24)
      #t
      ())
    #(481
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAH_JJqrqAbYOMGh0A2dIlnmDqHo7sZoZAVGKrIqMqW2c5Mrv2P9dOqz9jNIpsIuDW5ZSyU_AeohG7dvC53OZtZx")
        ("y"
         .
         "AFBSwfRNF8M05SKg2OPZmuMvpI32-5EXvuQxt6N0QDEJheX92CFwgXsPaMC7cwDSF4VORftpS0b19QMAYnOQkACH"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 225 122 158 189 60 230 161 158 94 170 248 137 177 32 172 30 56 119 142 28 17 41 195 131 254 194 191 151 128 209 196 18 235 200 251 194 155 6 70 215 123 132 228 84 147 55 110 153 29 89 84 206 59 60 175 71 156 248 247 1 216 131 81 174 209)
      #t
      ())
    #(482
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ANLLJDvfxf-yusv0NxYfAs8ilx2Lf5zQ5lwXSYn17Xycx1KgxqdxncG_PLtnr1zWAl4xOksMRkKerBMgps6yedrC")
        ("y"
         .
         "AYCTyP-xdnsrUBCeQPGNvnSKNX3h4oRmSqexCo3zCSqB7cGInRU5ZesuT3sxSCdRKwGLqvOSWB4qSZQfYGE1ZI-M"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 255 92 175 127 214 218 65 161 168 187 71 222 58 8 56 131 226 227 130 71 124 31 241 214 236 170 68 161 99 231 189 55 91 103 215 1 55 99 167 118 223 56 231 150 98 49 10 116 135 58 76 230 18 109 46 198 246 130 99 242 72 121 105 151 0)
      #t
      ())
    #(483
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AdpyMw_QDrr05v-KMD0lwlJ9_xnRDY6KrQX9dZ2YBz8mLmk84vxJdHrtKeJ-Mj5TXUafD80aih0IYE-v4A0brcSz")
        ("y"
         .
         "ALfeE-fc7bc3YASp9YmSLxh70b5y4t54k4c9I5K7uEqocRAPi5VUj134lmLpi2McTuTQVYCW1BcL4MHiHMBZVYI7"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 166 77 160 80 62 45 245 211 131 131 125 5 205 67 110 166 39 56 30 72 227 105 88 145 23 90 200 237 233 22 157 223 87 59 102 114 108 40 19 34 119 190 218 132 187 71 242 121 43 65 17 58 13 42 202 113 8 70 24 245 93 170 229 95 121)
      #t
      ())
    #(484
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AHf-eVgRlTXKbBp8h7tNybk2Z4qzt1fngWk4vIV2Q2BuYWR9IxudrT4D_E3Sms5fyCPhXzRk2OI21YYrH2KAyDaS")
        ("y"
         .
         "AGG6c2wR_6bQ-ua8jz5mur08bXm4nuY5RZ7GWnHxLqvNrKRHqZMSHcY93mrgzOvkFfS_5g_j_sPS9IoIS3WfB4rE"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 64 149 44 39 58 181 18 227 165 137 52 227 117 79 39 36 99 64 126 97 137 185 193 226 89 194 127 109 67 60 72 39 83 45 246 39 116 90 205 166 79 209 121 147 80 139 253 239 84 113 31 87 92 255 173 97 199 41 232 214 221 135 132 32 79)
      #t
      ())
    #(485
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AR0_8U-JBttgx7wJx4a-aEd2i4aElXvzkoxVa_XOeozIL5qw5jtL1-uHdns9smJJV8PxA-h7XBl9yMbSUHMz6lad")
        ("y"
         .
         "AEq57u6rlQCG4DD8YLay5HlzWCooLjsBnmJ0TxnGDKAgjjpTfWyjUyxZp3xUC84DmZiHSD3d39UktvnXv7axE5u6"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 26 58 67 67 164 41 91 7 212 178 143 113 105 124 175 126 54 62 47 60 162 5 235 184 40 196 77 235 47 91 179 217 81 22 243 110 37 233 14 176 4 36 201 136 11 69 83 85 79 240 31 56 181 44 179 77 166 67 32 125 111 41 49 47 148)
      #t
      ())
    #(486
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJyetMxr7WidqBXwXjzNJnVHPEJ9MRWPEhD9imr8-ycpce8KBpbkj8Ae2dgsmx1L4Ib6WhNjcCeAg42k-9ZIFTbT")
        ("y"
         .
         "ALOHvbqCpf5Xcy6-0mpIfQMVcsIjQDwGMDlhf45cJwL2AuS4dSTymDkq4dWGL6D_mS5R_OfxRXoJkb8l9mz4RWOe"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 16 62 218 112 123 77 94 154 74 220 23 184 140 195 65 150 103 152 182 76 36 44 209 58 189 243 25 183 124 185 239 47 6 129 110 232 32 137 130 53 25 143 98 7 30 232 181 193 115 94 40 141 215 91 176 229 213 71 146 135 160 45 33 232 51)
      #t
      ())
    #(487
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AZk9dPCkoS0oBL-eJXUvq53ArmCrxnct_hCvsFazJh8U_FFYS68lY0CzZSEg8k-scAtyne3sl7QhoekxwXxSjVpD")
        ("y"
         .
         "AJA_l3guwcWqzXw6sCCC16L2MdmUTcUsmtTgJYqJnvszvYEGitQ9fcZDtNfyOaesQqJvTSeAIVpoSV957agejq1k"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 8 122 217 125 113 33 146 140 142 53 226 254 145 95 140 144 243 69 13 238 9 211 246 118 28 41 4 38 199 67 24 232 143 208 68 100 192 252 192 67 33 113 102 123 87 29 131 74 231 126 204 168 164 16 55 150 133 211 11 140 210 141 192 75 215)
      #t
      ())
    #(488
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AQfYtjvAlYUQ7dq58R-jWuTl2XySMEA42Own9ulwBdAUPpacVBnoLrpQkzbiNJjFtz6PCyHj2fXLVhYJZp72eMfU")
        ("y"
         .
         "AZbFNbYjvm76UE6WnqccuSUodgh0DQSss0K0E1iCeD240lWm1Vb_wWUkur7uBtHygKuBcYSOfbVLdqOzHkFXIZYU"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 116 195 50 190 194 2 131 114 218 200 43 183 76 63 59 75 208 240 166 82 29 253 240 204 232 119 177 169 158 80 110 240 228 253 155 206 253 51 101 66 143 37 126 209 89 93 206 219 239 233 153 40 231 250 122 248 145 253 193 182 255 157 225 37 54)
      #t
      ())
    #(489
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AdP1cE28Z_4-nsOgJhLt8EqC_PnVpBxaMw9Nxlt8HoqSf-3zGlKyi_4rhOCPndmKJolEBVPdiRn93Vbtc2udBkzD")
        ("y"
         .
         "AAF5qtojIASfGEMNMqZYiatamED_QziPfiJTvj4OpB1qvp4zsMioBcgMUvpm2rRfftIEOQDjrajQJWSMSCJ0O1GU"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 141 159 127 115 32 35 210 248 112 151 198 245 138 252 124 238 145 198 123 61 243 179 24 81 123 187 64 38 100 245 130 148 8 217 17 176 169 180 28 178 247 220 185 109 79 186 54 217 15 2 117 224 191 107 18 83 209 124 227 242 103 167 181 107 255)
      #t
      ())
    #(490
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ANPcYqBwr4GtHb4C_wdYaKo148O1VfWNpEheFb3nibaAK1eYh9dTqhNYkWfdJrEkiXNOgKNJQcY4wQL6XBmeTREY")
        ("y"
         .
         "AZCjU0EguP9M96k4YYXNLHruZmfxMM_xsGAkLTQAKcFet9Uh4fapZj8Zsv50C0OEvzSA52y97h2ne48cRbnpJohP"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 111 18 149 49 198 113 56 128 16 212 28 147 74 37 146 213 125 227 55 109 231 171 253 50 219 27 66 179 79 219 19 229 200 55 29 184 195 97 1 234 53 226 109 4 225 155 168 145 176 129 106 130 28 81 3 161 246 140 157 195 153 177 228 237 80)
      #t
      ())
    #(491
      "edge case for Jacobian and projective coordinates in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ANUeMWNJJiabKgiFyCilq3ax_k5ePZQdlnjkuweQAr08jmwfFX5jwnVzHVc3kUKo35WH0Qt1_-YutFMOdddjQCbX")
        ("y"
         .
         "AAeA6LzqjROZlD9b0PT9GDfbL5vn_EZp0EIsQowiPaU7lWbvDSe7ZMrgHZ-UnRp0TosOeSeA0aJDoHk5VBgWbRdr"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 245 85 4 241 113 78 7 112 42 203 28 89 77 210 70 96 63 183 31 17 228 157 229 144 106 207 109 195 190 246 162 64 46 28 93 173 136 49 170 24 137 69 49 202 224 135 162 221 164 15 226 219 88 45 60 32 214 2 175 98 220 18 183 226 149)
      #t
      ())
    #(492
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AZ13cNXZ2-UHhuyQnQcfEtC20QyLHdv1RtDm9KP_fgR28ERy7uM9h3VYZQTwBegjKc1rbK4_YwEqN6sWxH8nzrNs")
        ("y"
         .
         "AJ1rZw6zP-_K-hGFf1w7q0--QcF0ZQ4l5JxlrGPN0sKo-PYF1-Jn7CAj85mWuOgl3QQxgiEI4sAf8HdX0klYBbRL"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 182 58 8 168 254 168 32 237 140 1 240 184 72 37 73 161 54 6 189 149 167 243 104 159 223 202 108 63 78 61 52 156 125 65 132 127 19 77 6 250 188 1 43 255 233 140 198 225 110 245 115 181 52 179 214 71 146 209 45 12 211 241 204 241 175)
      #t
      ())
    #(493
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AfODyh-o-6TsATGSfmaIo3Tx-_BPLnk7awYx0Zwk7GzXXFLQsZ7CfDd6SsSDS8mQAWJCBFUhVL6imFZjAGQ8Yyh8")
        ("y"
         .
         "AL2VYX0WVdzMW4rVD-exwAvVERsAM41uWYegdC9gi3SpNQKxJg_Rk7O9OpDIQ8E0SvFjh3MYkDZjfD11BNUOgGVV"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 83 83 192 140 167 196 132 226 122 62 4 193 216 223 238 236 172 23 60 43 110 42 93 188 176 218 82 35 230 52 189 45 82 125 139 52 186 14 89 44 175 157 98 52 252 84 69 44 95 181 5 131 58 151 112 140 203 141 235 196 223 93 202 203 179)
      #t
      ())
    #(494
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AF0R_8A8CK4yPLk4-28qM--8vWGmWEDDOyByZeyK3Qdk1Z5YQKhaBiv7H3BVWcGVSu6W4jyIjgq2cE82K_XFiIL3")
        ("y"
         .
         "AT16SvOzpYrRlhKhw3EjQ4jYwG9T8Ds56EwiVXzGZBpVgGh7sIYy7tMeDMEU0XnaaGU3ap2bDuxnwcWY7pcrdxtu"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 116 15 118 200 210 187 215 23 159 190 214 20 18 63 226 253 187 133 254 130 32 48 11 64 203 187 83 157 71 84 197 239 99 247 178 188 39 225 41 71 142 122 206 236 131 71 19 206 186 39 226 246 121 83 68 55 45 60 77 147 90 156 158 70 150)
      #t
      ())
    #(495
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aenjlf4bCxZiJWpLWVPpiXHc7uCYs0-fqofAehXi2WGOcT-YzE-OidZuBr9_a1suASSRhRsR1xFBOF-ptDNHxII_")
        ("y"
         .
         "AIIFo5G7A3UMeVH0EGkd1Czz1x3192uYwNCCRgwh-WNhATlSIk3adGPN4W8kLDTGJnPZFWrtcmDnsBdX7ua-pg7l"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 115 66 83 232 213 233 48 140 213 27 240 65 4 238 8 209 99 172 181 144 177 169 85 137 100 71 184 162 85 252 211 193 37 107 78 134 185 243 22 239 244 215 97 59 56 41 245 120 89 97 237 164 238 158 221 237 164 219 77 96 175 25 227 25 115)
      #t
      ())
    #(496
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AWQBbivDb6nvVnE9qJrPCwSAgtxjEVZ0Dsjs0iwSOEiDcShDRaluzvbc3aLhO08rlJHDhidIl_oTxrIxPFAAQZdV")
        ("y"
         .
         "AckKQFQxAFUyN3wgS6yaDZ0iZUJYT748ifeFxwiiM5mhvRTao-aMx2rtn1_Uv75O7KtKetKgAXDa035v5W6SXmv-"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 156 98 52 23 27 76 67 247 159 240 173 149 129 46 54 176 120 74 213 146 83 210 14 160 203 152 99 63 176 99 255 216 170 34 162 220 186 41 212 17 108 215 7 55 193 177 246 235 171 71 49 197 184 164 85 120 133 151 166 7 181 164 91 213 71)
      #t
      ())
    #(497
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AdNtKz7EJabCUHxN37W0oQ7E1KFQxcK4CiY7rd69MZh5fpftfQzKgYz03dR_YTS3ptKwoV7mCXP519oiD2KADFRn")
        ("y"
         .
         "ARu7X46d9lubLBBeTY9tvcTyPyVrU7R7woksKV58vbSq6BhYumZEMlVlm-qjCK6kxQnJnVeP37hz9R_1NoGupiLg"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 206 193 240 39 161 13 228 166 144 94 163 47 83 72 81 162 242 61 220 158 126 205 61 36 184 184 139 224 31 118 57 202 199 132 124 154 189 190 116 108 23 26 145 61 103 25 215 194 160 192 53 156 64 54 54 11 225 231 94 139 47 122 170 103 142)
      #t
      ())
    #(498
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ANtjvVruU71j_OBUBj3DZKrk8yD3rS3ylBQEMa0VUMfqs3GhPU27ePY9cYjmHcjiaIiNkxmQaijzYL986GjrZ9DH")
        ("y"
         .
         "AH1mNxFSlU9IWB25HBUz3fOxSLfpal6ZZPcGyz9u6AV5P9fFdn559CYjMwjI7_wdpx7HN0Skmmn7X2YkB_9U2Uh5"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 95 144 14 251 186 212 151 238 144 75 128 252 224 112 41 173 63 151 156 179 66 61 199 209 108 156 240 133 63 137 164 135 223 220 92 195 69 48 175 168 204 223 90 14 55 110 39 138 9 27 212 246 7 124 148 29 237 111 27 15 200 132 1 0 163)
      #t
      ())
    #(499
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AVaB93hrdwecaqnM2qWPOensThTiTRuz8NbLVr97eHYcO_WMf7dqIpQ7sl24DrCvdBDWBxVO-ScVKokh_e_nKI26")
        ("y"
         .
         "AeWxnIJ7KIVY2i31-fyjE37QaUB40_YAqnpJX-KPQY1Fi8VSdr99KWmsJCnir40F1BEu2-k7BB8tXVak_uX7kYtp"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 36 194 60 0 211 3 41 6 86 17 7 91 2 229 92 60 102 123 190 61 101 22 15 161 137 18 138 244 85 36 14 182 119 161 238 7 202 135 137 190 122 142 208 116 206 23 80 74 89 3 217 167 82 104 26 254 99 66 176 84 134 108 187 241 153)
      #t
      ())
    #(500
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ALPnyXdMUju--FfVn4zOzQZ_0Yx9d3fILGgUazOOFMXQmYs7liNU2bEHM3r9QTcjMEets1K4seIV_Kwl8eM5WUB3")
        ("y"
         .
         "AUjlztWEEBg9FccZ-K5MGdlfkQ81g95uC2O5OOLV7GcHVdwky_CPk0D5FMVTS_fxSDlHtrQGJvHH_g5NjthDAnsU"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 122 148 13 202 21 177 219 154 30 63 84 192 170 142 176 114 220 225 22 80 237 146 10 41 49 198 139 147 12 52 181 129 29 7 108 10 61 16 57 122 24 189 10 122 135 250 167 138 109 152 79 180 139 62 229 61 32 188 218 132 203 46 167 186 173)
      #t
      ())
    #(501
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AK_w5SFu580RXMYwkJ0g_Tw4I1fwdESA2GsXevT_lHGh42BTKpKlglLZPR6harR_TAkLau_cano3eNFcaKhmdYua")
        ("y"
         .
         "AJjiPENyNypSe-LGwxFzgLSDbV69CAe4DNnG79lqfPB1fCpjW5rW3-ARV_-S3tMTp4ytloCCrzPRNoc_gI2OSRYm"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 185 132 71 240 195 165 125 239 89 44 192 89 156 231 64 111 20 129 134 249 76 211 7 191 135 228 171 39 61 58 155 90 230 54 155 38 105 44 171 92 36 188 229 156 19 149 152 68 7 119 131 236 254 183 119 252 243 206 153 143 101 106 35 130 247)
      #t
      ())
    #(502
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa0Tr3novaMxIKKpJLSBABEXR5yPvszQpq9PUO2_ffjjleMv9qHlssG4dDNsRMZeHu0gnlR7PFdhyrlvVtrxe8aT")
        ("y"
         .
         "AChr1rAgbLzHqZ54B_Fd8P-DzBfkR08XacRz5xQcOEuKOfW1q83PxwSXBXrn4JtwesOnG4JKVdTYZIxaldY72CJB"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 163 205 144 11 152 133 98 154 232 216 63 216 20 62 125 138 41 28 217 51 22 59 198 88 243 121 19 81 24 55 53 80 130 213 243 148 37 234 50 43 25 214 29 179 221 160 95 37 94 20 14 236 78 152 167 161 1 33 235 41 24 220 241 38 166)
      #t
      ())
    #(503
      "edge case for Jacobian and projective coordinates in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ANTXa0DZcEbM8C1b0TGlRS2RGVoTIEtutPcaXanckVb2Ga-L-uj7rO1HXCfKiUVvNeVHaIptDZlI_qSe9LdbXoOc")
        ("y"
         .
         "AXONuLPT__satpf7yBRqcM3WV9MHpubKijOGYaCBMeBcaA7p2kyIqQ_elsVjAifC_UMj8wLVOgtRIRY8zEC--vUz"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 147 29 138 163 140 83 13 184 213 231 125 170 64 109 22 97 105 201 206 183 76 207 112 32 150 86 115 208 145 51 37 60 162 48 40 126 201 153 116 71 241 204 187 187 232 104 203 115 234 14 40 244 182 126 212 145 50 153 94 117 244 23 131 141 87)
      #t
      ())
    #(504
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AQPzbsQl3Yjl2C0fjXR8k-7MS0asmM02T8Z4vA1sefi_H-C9KCNe5D-9DcI3MyzC7W66jHpWELVlHJ4PJkRYfKO8")
        ("y"
         .
         "AU3ej76DtWlzm4YNex7a3Hv3P38OeocC60iNIw2ihDIs4CDpyIMSmNoUGAqwCEZebe0fHrr2VkDZLM8pq7Z1Gm9s"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 39 12 254 155 134 157 136 231 223 246 212 95 199 82 164 172 114 204 222 115 207 18 236 34 124 39 67 40 176 1 169 242 77 146 176 12 160 50 111 225 157 105 87 61 91 62 53 248 34 204 191 76 131 163 179 191 110 49 83 240 152 66 129 179 182)
      #t
      ())
    #(505
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AT-MxVaTRr8zJdhAX93p_ecd0elTwQ7WIVtPQBD1u-FzcYqOLm2fgCcmzZFuFuoc0xSMh58Kzr2NsmKPWJwZqqW-")
        ("y"
         .
         "AVldZpz6eGv-udzazt9WPQQFmGeJj0LooVfJETPJUql9kDiYkbNkeHXIIutI92GTCv75sGiFPv7A0mDY5R28ttJL"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 77 240 78 57 232 191 95 108 52 71 47 148 246 244 40 150 42 200 163 157 39 51 119 208 0 114 49 250 127 95 121 237 137 98 105 246 91 248 78 202 68 224 115 156 216 185 159 46 103 151 126 239 215 104 85 61 70 76 121 231 212 217 10 12 232)
      #t
      ())
    #(506
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ATdcxOGpKFE1mAgaxOTAYywkvRmX5IUNFHJQe8cUIvzLZM2CPTNlwG4_CMn9bm5HamgSDyA-9pyhHHCInjgJ63Xc")
        ("y"
         .
         "ANHtjXq3SnJGI2fLk-hDxgOJv87rL4aaozSRlhxLg4kjj12x94s5_AkjpU2afgvo7cuVcrLb_wpylYU4K_-mkBkl"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 211 119 43 37 44 155 163 95 243 71 43 40 10 118 53 202 4 81 62 208 46 77 140 37 89 108 144 12 55 153 77 38 234 119 71 55 217 169 168 248 97 138 110 240 193 1 147 68 203 63 150 86 166 54 157 251 29 143 42 63 40 180 240 89 204)
      #t
      ())
    #(507
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AGtsQUnOO0-mGeNg8JeUJ9LGv-s_pBIFoM7TpDcofHEcxuWHWZLOoxOuPS7A325CF8jEK7z_pzLEA-e0Rx0M85Uz")
        ("y"
         .
         "AK1_dMabcwi3h0f3O04-ZuM7_ksh1FrIKgspOw9jWt2-5Cpxi3Jq7rnfE1VwBJeQDZzdCkLgIozReU92fTv_scen"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 197 72 203 223 219 64 84 29 171 116 76 190 92 69 217 106 79 13 28 242 229 55 211 58 159 180 228 103 92 30 245 69 130 193 146 148 253 23 197 250 226 197 26 155 24 195 113 149 224 234 192 217 181 255 71 166 67 223 55 90 112 141 236 113 161)
      #t
      ())
    #(508
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aae64IcluX6K09kYQ6RxTZIFAA3t6oYHYQUzBzC9i25aaDBJSWzZ8IsyHLLz-KBbrfI3C7kOoaSSNuP-_ZG5w9ds")
        ("y"
         .
         "AIxQJEBAxlvLhs2-GT6vbaquK6OjKK3rxrcsEnNmCKuvnB3n57NRyOddSrgB_91uhLfka1CXYkfDJRW_kc6jNIkG"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 82 198 193 184 43 142 38 247 237 231 5 168 180 132 6 139 80 26 141 164 105 202 165 194 3 190 1 129 67 97 141 152 79 169 66 27 19 94 26 226 33 42 39 203 187 206 58 116 87 128 209 132 112 89 213 108 60 61 218 71 72 98 167 144 74)
      #t
      ())
    #(509
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJD3SiyKXSqZDN84sYXHF2s37lC3BVnthdr_L9RblLf53zTOV-6mVEJ7-tiZH2pdOFNCrzpWiLH4xyT7fnjcGPIO")
        ("y"
         .
         "AWFB8re8lCKG2-g0S2jg-aLQlQ2mUZH0frpzir7yC50QfMDMz2C8kpm3Xw0UFHpsen1a5noAh-sfS0iieA7AhkKL"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 19 222 32 174 166 145 245 211 139 128 186 217 60 128 217 35 119 235 183 48 32 20 246 76 205 137 63 169 230 125 56 74 78 209 165 227 252 226 200 75 165 252 55 154 10 107 218 205 182 23 101 148 223 135 109 61 19 253 241 220 130 77 168 37 165)
      #t
      ())
    #(510
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Afe-Ip3LNeRE09iRuwT_Wq8nDo-NkK_WWvscVvjHfqs9MuVdqjHamuvadvtnKY46G7cVBdOltsWvNzZsiAQQkOlu")
        ("y"
         .
         "AM8-Ey_Yg4T66HU64yumaelnk7adaoFTZTh82dTSGtKGLksv_KH6jqBeNCdf6pX_HcyrdQvHQt4SYRzDoTWp2vnA"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 32 144 132 114 147 186 128 220 122 138 77 80 56 173 111 62 212 54 181 168 46 122 74 109 124 250 234 28 25 214 199 77 63 112 127 160 255 201 241 90 140 149 195 224 64 180 241 181 210 181 238 8 185 67 100 16 227 111 218 32 63 214 197 252 127)
      #t
      ())
    #(511
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AdhXiyIgkhA7sXdk_aM0l0sSEwXEwc-u59hugU3uMwV6A76iNFRmBc9_ulDS1Xq9CAh2Ck-NI_ckwA-fXdSwJvYo")
        ("y"
         .
         "AZIzJaN7AxTHodGFFztIwUBRvA78m_Pleud4ulN-kMgswNQt2Y6f9agjXt5cQoZ-lh3vj59mpjEWjC46EJnJwt4G"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 153 170 175 119 122 108 41 184 94 14 201 144 47 79 141 0 74 41 123 69 112 221 70 119 98 35 5 244 51 158 199 38 175 185 139 85 65 117 185 155 139 50 67 241 42 96 183 159 254 37 148 113 5 62 20 199 104 161 140 191 154 229 158 20 155)
      #t
      ())
    #(512
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AbrfakEaQFZS0-VLj7LFqu2Oua2p8G7_wuZSmQU7ijIW4LXueyVlYd-hd5lwVHsQckJLhrtu9AjO9XW9sCt53jXU")
        ("y"
         .
         "AU7zpHwlJlscsvKNWWP1M_es-9401F-hrl-ud7TE4MiUKAJHZkqtkcMaAR-6UOtuNL398o8-QGpgLFBx7rtjLNvl"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 107 116 202 74 88 143 164 140 197 254 115 138 168 73 82 231 212 144 142 249 251 105 164 188 71 14 183 46 216 107 28 107 202 11 99 20 147 156 49 27 14 61 174 127 237 216 218 249 106 55 183 66 13 197 86 172 43 188 189 13 253 218 40 23 72)
      #t
      ())
    #(513
      "edge case for Jacobian and projective coordinates in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AYuSN3I8ApxVGx4USSRg59LklX3O7lNt7JK7Nf6O3jbmsczhVbaaDXISwrTwuonT9vwOf2d3_1wu7Yxx7_PETahI")
        ("y"
         .
         "ASW-54A5-9PDOaWLv2JeUDRqPoqcc0YOxo-wKP1NFM1jFTEOAxGg7E85-zQIUZN33U6p1Xd5ADhiwxK_0JqaH5ZZ"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 216 28 129 46 20 128 77 86 46 74 137 233 238 167 210 99 2 207 215 85 254 146 33 252 165 119 227 47 213 188 83 78 63 178 216 27 23 1 175 160 180 217 242 91 189 27 45 126 201 132 218 64 64 20 181 194 61 139 41 88 174 216 100 219 127)
      #t
      ())
    #(514
      "point with coordinate x = 0"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "ANIOyf6mtXfBDSbKG7RG9AspnmSLGtUIqtBoiW_uP45hS8YwVNV3K_AaZdQS4LyqjpZdL10zLX85-EbUQK4AH0-H"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 127 222 222 148 112 128 21 104 41 142 10 148 205 73 160 232 23 2 182 244 239 145 105 131 255 137 228 240 26 222 46 225 48 1 198 234 166 119 73 154 157 231 164 139 124 121 86 250 172 88 14 99 147 56 83 19 77 201 104 82 38 79 178 63 44)
      #t
      ())
    #(515
      "point with coordinate x = 0"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AZn_iayqYNDkecgWhzaneV7gwbXAsh9sv5DjBoLPRGTfkjI0PJHzZQzwH3vCsUjY87cH_S49_1ArygwDlBqa_mMc")
        ("y"
         .
         "AITicVXAkD3-Zde12LG97CRWylDJd6Q_ROT8TdcNlLKcRGllWYGvTwwhgfGdxEgTB-aeIG1OClnUjkP1WAkTmtp8"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 95 249 208 227 59 25 161 237 101 64 130 84 149 122 67 192 5 11 25 93 253 143 235 86 71 45 63 239 196 99 217 89 16 134 47 155 176 163 45 152 5 55 99 51 63 146 51 38 55 218 190 42 79 158 235 207 72 164 99 14 197 11 191 19 45)
      #t
      ())
    #(516
      "point with coordinate x = 0"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AUNptuZxfghUOAgz2XS-6pzMwB1_vMOSRbwUJxBMIUMmgJ_Rpnj4lhLQh99ePVqxhVF44B-8ZxJIKzRDzbv-w8d6")
        ("y"
         .
         "AH5ah4aqyudfn35Zs7AM8Tas2sQv8NoGoN6C1ePEFpqbXWPNfAxoMIWF9vMaMA0zwBAEK0TN0OaoEa1hTvGLbU4G"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 1 74 54 185 138 181 125 29 89 147 81 39 43 49 49 253 18 225 140 176 246 101 204 152 5 178 64 44 103 14 227 9 168 91 110 12 42 156 202 40 213 164 88 60 204 205 231 184 213 170 243 12 103 128 206 223 138 11 154 79 106 73 233 193 233)
      #t
      ())
    #(517
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ALCN8GEt272Kvhyt9lT3sVAJQpccsX-2OTZ1Awi98-kdq6DiAFV13n5wOPg3npl1ltZpblciSJ9_1fQahfmyeyBu")
        ("y"
         .
         "AG1PNVjshY6Le48xr0gKgEhCI6bzZShoo22KoEEuedV8XTIpfYC1CL9aG7UvtDkWOswmtHMDPcJINOCkTfpDSAqe"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 84 21 213 74 189 184 83 110 26 226 172 0 5 30 42 243 192 66 168 33 83 1 101 209 254 187 110 189 215 71 129 235 211 56 127 157 216 132 93 49 160 51 30 78 181 66 8 91 97 240 249 244 8 186 91 70 53 221 36 218 186 218 239 71 1)
      #t
      ())
    #(518
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ALV09t9HkyoYJ-NslEHpphZwlpgXc1Yy8eyPzeYFCfbMmaXGUwqWAD0DP0q2ODbKKSZSuGBkGiuY34PO_yvyQZ99")
        ("y"
         .
         "AWyAdC4Dkt3nAWsQbj-5dr1dj0ao8OdLkAqNJvaSiwLU_BqX2EhEwjgPb9Ykm-u7puX4f8XqLtwTYtd-HCRmUaVs"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 229 163 58 160 211 70 186 87 134 103 65 249 144 17 241 69 173 191 45 252 193 12 188 152 140 134 232 162 110 151 123 65 154 199 72 177 6 241 79 242 253 238 90 50 102 22 171 83 204 190 45 128 232 9 129 19 119 254 154 242 3 44 161 132 100)
      #t
      ())
    #(519
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ADF0w9NAd_lG-Jhq8hjKAfMe_leiNnGOuI3yajzMsksw8fjp1Pu6K5U-FhvRlEcDnbrxvOBMNGDh46IXDiZzAtLf")
        ("y"
         .
         "ABDP4Yrmy1Nqw-FLPmDMvlKymhlSpHtbOx9MJjetpTS2Gl2U9lj8OGvu1hJGfz3Ry9jk1tFUVmq5lDKNKljmnKeo"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 135 121 203 186 23 24 189 43 127 194 172 92 180 113 48 133 118 37 10 212 18 165 249 142 40 209 77 118 218 254 203 132 85 96 59 178 69 145 184 232 62 138 92 44 85 5 96 148 69 191 122 1 154 34 152 97 53 29 22 88 1 40 91 25 232)
      #t
      ())
    #(520
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "APh7t0936btGEa953FguNopR9F5roeDzpW_yWmAKWv4KIO32SWIE-9LnOppU2ciAf8C5UwHfGcp9ZymfRLKAvGlj")
        ("y"
         .
         "AUTNVDWLMBvzQeew237_9LABDxeSYuX6GBsw0ZE5Ep9CRktx6i4GiLoyRNT3y-L3qMKis3nGSnLWv7-H9vGoe8Br"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 244 85 129 106 203 89 230 24 158 108 20 218 215 167 121 149 139 155 96 169 84 151 40 214 40 176 175 26 56 190 224 41 167 23 179 17 251 43 108 234 95 243 200 174 160 150 35 108 103 198 163 172 224 66 100 172 126 209 190 234 42 79 94 144 68)
      #t
      ())
    #(521
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AFggoStSmqOQavsGEgPBJNO4HzmbZ_SPXMO1_UWQcFHOBTbb3utLPk6j4sN5kb9ovpoaHGCaLse0q1XcAG6uRA3a")
        ("y"
         .
         "Act31994uYdEcMPdJo6RmfmPgylTrrewdEnTeQQFTJ3SF9ry8K5tsFiXUSPJ6vnYhtXD1gNpB6XP88ccenqF9i8L"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 32 7 253 152 180 227 52 217 221 118 134 174 93 163 121 195 195 160 81 34 50 132 79 160 14 206 118 234 148 33 23 139 156 21 73 66 192 152 28 162 184 24 113 97 213 154 211 106 178 218 241 117 62 59 89 216 250 91 76 155 242 36 167 55 125)
      #t
      ())
    #(522
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AFjg0g2KGI1IylfUtz6k6fRTo4nV2PuUhzfvVe7DCrYPrVgNnhK7hA-C50lQ2j0SvnJmRXu92UO4MfitDTtX5iQ0")
        ("y"
         .
         "ANcMJ6ucsp-ddTln_6_SdVHHihuuR0Qa04IEE0-DSV3rGYJH2HXxmvmTIsKTc5A0CzJ9nmpYtBx2oybG777ALJlW"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 25 158 101 224 111 124 86 243 196 122 175 159 182 101 31 93 47 206 35 250 75 147 162 29 187 255 150 85 155 73 27 180 166 12 178 185 243 91 123 69 195 168 56 130 138 83 197 71 84 54 86 198 87 201 22 89 122 192 76 192 5 176 32 91 206)
      #t
      ())
    #(523
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ABQlHpuHzoH38Ul3DAB_HWMgstgT0nVFdzzP9M50hDVP39Gfk4DejM52zqvWUwoMgSo05EBL2CJpzQEuGhfHHJ1m")
        ("y"
         .
         "APvNRDBSz68DdgvVNmN4MI_da_KTefm2tnfJ6IsDbvJqOidrZKdRng0BmgB2D-84fdSD0NWKs9MBWM0YsmDlBBWJ"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 46 227 11 176 129 160 75 124 88 93 18 207 46 13 204 233 73 56 222 176 183 42 95 186 128 22 8 99 133 76 191 116 149 252 78 131 153 173 6 170 110 160 89 87 183 98 129 5 127 7 1 25 107 152 51 113 174 236 14 78 49 246 180 43 108)
      #t
      ())
    #(524
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AfUYOK7jbR37sADAo92EdVqH1tzF7lx4yIsw0_4VSaiRggSvu3QqzZtRIPEJ2_354W8OroS8gsr0E0klL6mBKpRB")
        ("y"
         .
         "AeCzp2abs67lTmS7A7aKYigxFQcZoh1DKPcypklmaiWY1a0PYrAKJlqeCPXsC3tvnh-MraBDeFgLSih6OvSuOVMV"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 137 120 103 0 144 44 240 193 245 71 157 131 158 228 226 64 27 210 73 244 19 192 74 163 1 135 50 252 95 249 196 106 99 116 121 249 52 39 224 48 250 71 70 96 225 139 44 171 70 246 174 120 243 49 197 189 53 28 191 50 234 103 145 254 216)
      #t
      ())
    #(525
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AKi7e19CV6MxnG3LHfWJiM32BFPkOdIw9CUWMv-SskJLFHykCCSyXm7F9xWZzP0T6mITng6t8lDA0Npib08e8My-")
        ("y"
         .
         "AElPplk51fXRRM6lQt6ifXsm3LXV1ugTvFMHdTthhBBGWxuWvtecmLamC1igvfg2CMgantNitmpMYTI7NNKmQWyD"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 112 104 49 27 92 88 57 124 22 55 189 154 243 139 107 168 127 16 133 30 238 103 92 186 220 39 134 14 120 212 72 171 243 61 236 23 77 152 45 171 109 16 13 49 92 85 120 114 188 143 122 255 43 160 255 213 8 46 226 3 132 10 125 100 199)
      #t
      ())
    #(526
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AexJ3iHWkvzXCEKARg3BQQtc4UhV9TAhREe29T0DsBm44fp6hi6uVfqQGPyTV5k2dHyWWSyY7sYlcVcuS0CsgWXh")
        ("y"
         .
         "AUVzzuZcrk1UOJ6NdOEgswgpjxWwdaRO0PUOzz5KsIGsoVLGFMUaezEhF99_xgeoYdLa0QI3m7q7jXL8g27CyCMM"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 78 38 244 107 82 4 200 234 69 248 1 21 116 26 198 176 173 15 188 52 174 48 216 155 133 193 163 144 202 40 182 184 61 205 116 72 205 65 58 0 87 40 61 174 40 86 37 190 194 147 46 147 63 243 72 9 152 33 88 123 238 214 93 55 215)
      #t
      ())
    #(527
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AFWFglaP9oF1zsqgZSvvNz1odaGWow13cVY4zUUFnpwgdZX5K6WgKZCpt6BfyGSTZVGM7-okYB7BGHqbam_TfT6C")
        ("y"
         .
         "ACwlJJg9XD1MCTWZ8A-Wp6OI4tzRjd0kkjq2-CNt_NVEch_SLNryD6UeG_n5qemY80FU8mYkgnzSz6Gw5jTIMFbZ"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 89 242 214 82 62 211 243 115 242 249 85 187 48 43 185 78 251 116 44 57 252 8 29 161 185 107 54 206 95 63 224 123 255 7 251 210 37 219 169 234 42 130 195 191 248 147 155 60 255 236 181 132 75 224 236 233 171 229 197 29 191 2 18 100 127)
      #t
      ())
    #(528
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AYjc2M3zhVz3z39iJRHGSaqWgkvc_jGEhZcKsu6yQo5JrkbieeOwIRCDnVpEcfaiNq3udgNhBDsxBkiEiMy9LK-4")
        ("y"
         .
         "AEtYSsHTIjx6Aaclw4wYK0ypKFjdLnafgwUduVPiDGPUXWlyxmWfymZNlwi2lzkF5zBMOWxfc5oPxmgTrKrB7BsM"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 91 41 227 63 121 89 226 202 145 245 138 126 158 75 19 42 9 137 104 127 90 57 248 189 97 247 244 34 117 204 37 96 89 160 79 5 165 167 163 134 237 240 113 247 201 129 35 92 61 5 47 247 51 132 59 201 115 143 161 4 185 222 185 36 94)
      #t
      ())
    #(529
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ARPHHLO2sy9tIIeXjz8K7jbqjLHwKNLZgWH3dYu77rO5ZYivl7y08__8RXuQtRc554lPI4EW6YXKrP8-dRtWUYs7")
        ("y"
         .
         "AFtx9SMFmGieYlR4i4lAA9yR69lTov7b7SOgaLn5Q3mL_k0EX3sLrVNnJ6c6ZvJQE2-PUAZ1P_BLAKFIr75w78FD"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 57 207 80 9 198 65 26 91 113 176 69 63 133 180 31 80 107 114 220 203 51 243 68 173 37 114 68 220 249 197 36 202 168 253 211 118 181 35 72 11 162 37 49 144 66 21 210 108 138 129 139 143 128 164 170 29 209 17 172 216 238 132 205 69 61)
      #t
      ())
    #(530
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AXV1o_hYNMpIMr9kxZ3xvzCsgwQpHIFTl92dkMAq4FWWQRCBSxWPAp1As2sqhalWhnn7VqWrSXMGjZ7dgO23pqdq")
        ("y"
         .
         "AO-jyjEBFYaUulmdJ7TtDEOZd9KFXdV91L0xGsw4grCZZ69k-jSrCMryTSrKrzLZPKyIOdORqsUeLNBn9ARrX4fT"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 186 6 146 84 169 81 169 97 170 222 6 111 240 71 151 191 201 205 254 191 42 25 228 244 183 36 2 78 49 110 138 253 158 212 139 99 106 74 13 120 204 77 55 217 64 42 209 201 129 100 15 144 210 143 78 86 108 234 201 182 108 64 0 211 239)
      #t
      ())
    #(531
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AG3Q7o5D9hXjC2PcRVcBZhyd7Gnf3mWrfPlyEDnj5dPMXBL45MC2o0Ir8T0y5BCCVSkPnMnW5M-5yeoWX5NF4XWc")
        ("y"
         .
         "AEZJ8Dt45d_6u6cOdOlsU2L6gxhr3ESCC7Dq8bvRpRl2y7z8A6mGvd-TmW2WD6d6rEb5t6XDLGLDQCxACzFlwboh"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 108 75 254 23 201 28 58 178 198 14 92 156 47 172 27 228 137 63 128 252 160 86 71 195 76 130 238 142 229 227 179 200 156 110 135 188 58 244 68 223 221 29 105 42 61 164 90 69 151 91 213 7 116 71 19 19 213 183 209 73 165 57 91 167 237)
      #t
      ())
    #(532
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AabV0GixkIF3obGP4Hjfj4mUWHDKiBFpW0RwBIzgw0RDuaf5X4r5-2pMoChdF5HOl1rVplpcn2s9-AXMabOYTv7U")
        ("y"
         .
         "AQJTOeQKJqP29P5UEBcGt_bUdbkPWPfKWBerXD302DhE9Cl53MiNqweF6iQmQxWVKb0c7tm-Tu6yMC-sxrRAtYJk"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 81 145 132 145 4 205 229 190 37 99 192 19 48 101 255 203 244 191 65 26 243 46 9 134 89 137 179 148 236 117 232 43 78 70 73 211 49 37 3 79 43 140 174 28 41 89 42 170 158 80 112 13 167 76 8 131 24 215 32 166 143 88 101 62 18)
      #t
      ())
    #(533
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Ab19ZzebMJZyijZnlvcBSXfRHv-S8f0wmDx0Urgv6mH3euSkMYc-LFWIlRtmNQRhDRORuPkSL9eJbtRbJ9IyA4Kj")
        ("y"
         .
         "AZVHJgoCG9hrsUkDK608auGURFiZVzxAB_RM7IjiMuQ9Ano2LPWc8Sg2fN1ZU2Dgu8KiWsqGOwl3VxZ95EENJhsW"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 31 142 146 93 91 45 34 247 209 215 253 218 163 87 95 163 45 68 111 153 231 116 219 40 44 147 208 11 238 202 189 169 170 207 198 68 185 206 205 118 231 241 34 81 91 110 190 243 181 96 117 247 162 16 31 156 73 46 131 230 33 71 208 155 70)
      #t
      ())
    #(534
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJFD3MjURdf2ZEgfus5okARTlJU3-tc3VtgYVh5ZRI_7SQIrc3_D5yXWwcRbsLBogJIosP2JYe7yIGEr4FBAxUAu")
        ("y"
         .
         "Afi9BW9fn7MIPpCDN0hkcx11RBodxStBkE1rLqq4BQMLBM2m-0LqzMfJ1QZW8KhSJRIKBnGxiS8sEY57U45cVapk"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 114 167 249 158 41 26 21 196 91 199 140 72 78 79 233 73 3 169 88 4 129 38 243 194 64 92 95 248 186 199 221 7 221 150 212 80 176 23 246 203 246 172 246 159 225 19 251 122 103 102 222 138 149 110 192 111 119 44 21 200 77 233 244 212 98)
      #t
      ())
    #(535
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AID6pWhcGfCFEB8BDoOy8BAJYqGEeMm1GqDXUr0OQ5U3iSoQfDSlU8GNtPdRXy7zjK-CU60q-MWdhLf3kXLPLrTL")
        ("y"
         .
         "AenAMoJwt2GgyRWm7SnuIjEz60Zu_iMXxvitKzyNiHlOSro_lmtKylWUyadLN7RQPuWgeU1ZwQhmaJf17JvHu8vR"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 254 123 144 237 161 247 35 197 228 66 1 125 173 222 138 255 227 202 128 175 201 244 112 50 103 98 111 71 116 240 110 44 33 165 217 198 71 40 130 234 136 6 169 19 156 74 116 15 38 84 209 12 170 94 61 123 187 126 89 113 63 160 50 185 253)
      #t
      ())
    #(536
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AU1vj6FX7r6ovCcoYtn4--5j2lvpU-Mz0Gt0Q8u5bQxGShcZQ7glZepxDBJkmK63l5gjsusegb7T_91uQPRSHmy5")
        ("y"
         .
         "AHAbz55F1q5Imdlu1ZqAFcRUc1-e_dtHWY--BH6MQCHXhts1Odp5dgNYwPkosrcsuTbLZuT8gf8Hns3o4YmJ6HlQ"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 64 99 179 220 228 201 159 95 83 229 243 96 135 92 2 189 69 52 168 59 187 119 155 233 235 5 230 112 89 94 87 167 8 105 28 93 240 160 6 43 67 208 250 52 157 178 1 214 39 156 19 29 132 233 102 44 248 47 190 234 182 234 2 148 182)
      #t
      ())
    #(537
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AWNh0ZIH-tO5z2ig18siR0Ta9cGMDXnwYKsTadfwJsIdot6gmHOfx55mS87tBr2LjylHGQd1QxnO_Z8hbzImpKh7")
        ("y"
         .
         "ALcAey35oSPdgd9PXVITWAwXT9aKpd4j5ThsH3a8ZxG6v6tyz5v6Mo6X--mjt5rQ05p3jjxkt0o0MZrqBCP6JwBn"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 118 81 136 255 109 13 177 144 145 75 62 233 203 246 51 233 229 194 96 123 115 35 231 14 118 195 207 62 43 63 28 220 237 166 43 80 117 168 212 59 48 111 200 62 94 67 148 203 166 190 0 152 135 123 32 0 56 85 8 208 148 44 219 164 106)
      #t
      ())
    #(538
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ARI9i7F9hHYM604-yYOHR4-Vh9zAW0cg2INP_NgsBvWauieDZeg0vKJzbtYzN1f1eSAla9nhsb89Q7YgiW2_2Hej")
        ("y"
         .
         "ASE19YftamvWsJXUHHq8KnDJ1JRh0-189OeByCAFg6Do6Uq4oAtSp9C78YLLO8CDL-gsyhjlPniwSb-epqxgF-PY"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 177 201 232 164 116 142 110 26 234 61 171 243 122 231 75 175 40 97 228 130 175 50 48 157 201 62 124 126 172 234 165 132 65 100 69 161 236 79 15 36 179 147 142 17 129 145 165 191 158 114 136 91 186 92 40 34 117 196 159 94 75 216 46 37 103)
      #t
      ())
    #(539
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AGGqSqaA5aMn7gSVvv-ByalgGkpCqDByVTj5lA0jT4_sOP_SFTcJRJK1jsxBAu3f4Y7o9ZR8RzbcYAZvqNm4Iw9A")
        ("y"
         .
         "AXMClr0dE6tl6FrcacCCLiDn3XOXVbwo1yMeebocMSUMGXIlLCgIlvMKJsiAo0gEGwlY7gRDwuQD65goWZzakPeo"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 83 164 112 159 196 116 71 110 118 148 139 4 212 48 194 237 112 138 182 151 187 84 88 157 53 167 201 109 51 99 32 114 180 163 125 238 163 66 141 141 10 147 60 123 255 237 6 141 146 169 6 31 15 163 158 98 231 213 82 179 30 74 21 5 9)
      #t
      ())
    #(540
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AIa1vPS70FpbVlG8qXkeVQ_sPpsv8Na7JVstkZD1lGQ5tT-VE6Ei5coKn_EuM4a3Uk5G1_VhZ8phJ86D5nv13ObO")
        ("y"
         .
         "AIZbIIPVwCAChe5_9JUJIqir1O75PY3tJbU8-a8yfm5MMMRDjtLEDGfoeDVRg2HJj9dD2Fn2wXOpUalamUCegxlZ"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 103 208 218 76 93 173 210 95 135 135 168 74 150 68 92 37 77 142 23 254 238 183 210 26 246 113 46 48 124 215 53 30 190 24 92 10 82 116 2 165 254 218 161 178 106 206 85 73 171 19 178 8 21 109 20 56 159 216 19 60 111 79 227 30 45)
      #t
      ())
    #(541
      "point with coordinate x = 0 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AaCaynZclJtlar21loBVMWOEJFq3dehYQRlAXIWref3H7x4HmjW9KZrhSdZfFasNZJEsXJpivKQb21hngujrDP-W")
        ("y"
         .
         "ACWKUPG7VC3RZLiUO94fIHjqfT6JGB76skLaMLGhJWIYQQREnk11na6cudB1wwRWsaTkh0Dcyte1mxfcHIHVF_LN"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 82 143 90 183 88 217 182 17 47 25 225 2 31 80 137 213 38 204 80 145 16 38 226 125 68 202 109 119 34 12 129 107 35 38 162 221 107 145 82 64 136 19 191 141 57 153 6 203 26 227 58 82 207 103 231 71 9 215 150 199 7 1 113 4 87)
      #t
      ())
    #(542
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AKbiFqCbY2Ay857qVSGSwkK-f_VHj39_mwyqJbiu7ere0zuma1_ux-dbDePn_uFC8lDmy0x7CaFobKC9nPLS1Iuu")
        ("y"
         .
         "APz6_KI5GlWt4Kd_uaOBywrxYlPNO8WHI_hSdnC1lWdpjn_DoX5tJrOopKYX4rEubWA5H1AxF_6i3bRpeLbS0wom"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 217 55 219 47 202 174 121 95 74 236 99 101 190 129 165 20 147 156 84 131 87 198 20 0 117 219 74 7 107 232 192 211 166 18 107 114 11 16 91 61 156 246 168 48 115 70 203 131 47 190 58 147 96 23 166 230 32 89 186 182 114 62 136 81 118)
      #t
      ())
    #(543
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AFC4YqgqC6kqtFs5bFGqUZqkTRF_VXYOWk8fF337W83w1U-5lWXp99Z1fAT0lFauqHB1VgsdxRRb-kBoaEsEF4qF")
        ("y"
         .
         "AeHGilezjiKBgG89t7CvDjwrXlLV1dSJDFufXXN9-acG5jHkRT9uecvCjVe9xgSuqAEOtJH1wJ_rIOO8DA7UrkS_"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 203 235 92 157 240 187 48 168 180 91 193 122 91 210 85 226 200 36 157 191 145 51 112 91 241 229 2 212 231 48 237 211 19 28 141 55 10 167 252 120 212 107 34 145 24 27 180 12 168 94 122 10 198 7 124 244 54 121 39 216 197 182 116 67 6)
      #t
      ())
    #(544
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AFfyfMdAC2-bs6D3I7wGcJOP-Ij4eRcQCpMhFbEnU6H7sIblsiUgm09wcDq6N0RD8R_Lz4d5SR4il_uufxi-LU8j")
        ("y"
         .
         "AAan8E5N43QUnc9ZbZSm1--NMNmIi8dQAibte8hWBym3xLS_KPT5LX87JaWs6Ibovj7QGvAhacrqdl3HY9DalJXH"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 162 106 169 146 34 228 78 164 214 220 238 34 159 33 244 15 114 59 92 187 233 45 86 86 18 82 231 176 159 150 235 39 26 217 43 62 22 70 70 8 116 86 21 24 47 187 112 80 226 153 27 10 252 4 15 39 202 116 227 88 52 100 102 203 138)
      #t
      ())
    #(545
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOBKBNHiv6Gl9vN4Z-wlFFDUbmvSgGfLmaYS9u-r8fcscOr3LXDaq1OJDVSAbEYEd2YpPeCF8PQEYrL1asT8aBG7")
        ("y"
         .
         "AKIPoA_ozNiwB-_dba-9r5GxCSTAuNWDzoKSAshTPpgVRRcZ8Syv78PTvcpdUQJH1Z_9INU54iFl8RtdEOkS8fnk"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 154 108 1 125 174 68 149 125 182 171 52 118 169 219 235 102 14 165 78 32 171 120 93 90 52 30 14 2 101 5 15 222 33 121 0 113 137 8 225 118 62 235 102 83 4 117 252 13 10 19 70 198 195 188 41 82 218 124 240 52 223 24 101 6 69)
      #t
      ())
    #(546
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AI0-DJgg0rSXEXgISauTSd4pWFG2msD_ay7aH93BfpfHrCaV8o723cHhYGLuSClf_1cRzYcZkqihhLjltJ1xeLX_")
        ("y"
         .
         "AXCeQ17mx8z-ZpLH8WmuNX6U_uvK1db_Daf_VaGdFAkAaqYIP4l4JN5MDFm8gNQGB6m8_fZmuao4j6O4ZddsSSkW"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 239 211 254 255 173 141 40 55 61 255 192 227 169 205 84 64 133 255 194 143 91 184 6 151 128 29 180 204 185 131 250 35 40 34 42 66 158 125 99 103 221 212 164 8 17 172 42 220 143 189 142 160 110 164 8 128 157 91 130 251 237 60 138 149 203)
      #t
      ())
    #(547
      "point with coordinate x = 0 in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AcAuvVR2Wjgl17-tVjWpjZf3PPtX2pxCwfRkIF-sAc2Jn34JWhA5tH4Py-vWW26ixIanveSQDpVI2KZKDm851hyV")
        ("y"
         .
         "AfXrnSSRtC49AsIP-FMICqLbgj9gkaYqKmu1LQnTwWKjrQLgJC0kOmHOAGP_j46T7CGuykrisVK49o1A3YdvgOfD"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 66 71 132 209 252 163 180 199 129 16 131 133 95 191 134 98 225 240 238 82 181 255 170 160 207 137 11 185 4 21 5 18 167 90 16 119 234 211 154 227 126 198 125 90 60 3 210 252 181 249 246 149 249 145 140 244 135 44 91 74 117 125 29 15 226)
      #t
      ())
    #(548
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AEsE09HGxHBpwL1Elz51rHU8nyhLCx1SMIvN2aHgYweoznrwk2L4zGorW0sjSeKDCVarsRkcfoRYLbEijC7CUCHM")
        ("y"
         .
         "AMKvBeXDEDiMihaDXQYELkkKnqjEybxTLC59RvIAEHqX4PvF2frKncsiSVEOGZiWrDbgOZsPF2UEhet0jh__nHf6"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 50 185 118 50 229 123 229 85 134 232 135 210 109 173 65 229 0 191 252 250 150 54 132 132 85 127 52 124 75 232 33 48 45 232 243 247 43 152 101 131 77 87 156 246 150 64 97 69 234 181 18 226 249 109 42 165 137 116 143 100 66 8 105 18 29)
      #t
      ())
    #(549
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AM2i23_E-dTBzja6gqGEsvv-4gbZpdBQ0r8bL8ksDQt47KO1HXJKpfpI9maXK0c6zO3ElySsKz53nUyJQZjZq5N8")
        ("y"
         .
         "AM0ThX2Ehxg393pU8LqdGPbpQtHW0ucWf7U-NYWySUOCiavGiSesjCnWU3fMc_hdETUR8mIr_2l0ZSlPXNGjdyg5"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 186 239 247 39 83 13 237 95 144 59 32 144 249 34 180 121 196 99 26 252 237 151 239 23 128 152 77 157 152 254 6 164 126 240 113 78 228 242 163 47 8 175 172 90 88 58 81 98 41 83 156 44 76 130 243 97 182 241 89 34 45 92 19 118 176)
      #t
      ())
    #(550
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ADq02yHVZF9Cu_FiLibP52A78iKvhFSd1ISmLpj_7ybBPaYa61FXqR1wqCjpaHPiXwbEXWUlCTB7DJmDkflR1hHd")
        ("y"
         .
         "AU5ZnpTzJ1zmAZCqY72Mi27dftEe6xOF0NETcWhiTrbYElrpuFRhGHIBmIzBG8VXaA1bu7T1TCe0VJgxt7bx5Z6T"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 133 90 102 133 219 5 254 227 238 201 221 17 117 13 49 68 158 64 105 110 142 28 106 94 99 52 61 226 218 232 49 71 250 211 55 173 144 32 234 19 31 171 140 76 163 15 74 3 224 188 98 138 74 90 183 138 9 224 113 254 173 113 21 67 190)
      #t
      ())
    #(551
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ALyvnfWYbvpULXzp7qBeVXw5Ojo-Pd-r60dUnvmikk6_9Y9_tZidQEJYeRZZz0Ic2dLrHvbPIf5CgYKstyqts_3A")
        ("y"
         .
         "AD1jjv4WNjqK-GnuhdrRxvAD1PT4J6ehjHW9f-szATPlvSl6v1YVnFDA0Eqi47AZTY3i59D-TW16iQH9Ni4xDtTm"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 165 123 255 240 49 64 97 27 226 58 213 43 63 13 82 90 188 133 79 131 13 62 72 53 216 1 250 151 84 225 207 193 42 78 96 24 149 139 68 186 130 128 135 236 250 179 174 145 122 186 9 11 225 70 65 84 172 229 93 76 72 26 216 49 69)
      #t
      ())
    #(552
      "point with coordinate x = 0 in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ABGMBMMqdbB3AjI8oiakrOBUQksabm7c1AEaA19LwHifYVH_hJ7_iUlVT-TVQq8fA8ebNp_0w40dKbSjvEHV4Fuy")
        ("y"
         .
         "AQ9LjVEaxbGmU0qsndJIa4zQfPPVurwk909TewQRWgqPbXl3mPyAf7ACdGsniSrjD3UQNLaAsXI8LuhZva7QVA53"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 81 155 164 23 158 248 90 75 81 179 13 178 246 233 226 243 212 155 31 62 134 17 201 186 50 134 215 140 166 69 210 157 122 22 48 85 254 68 172 207 199 36 9 29 8 66 8 26 15 195 159 15 247 183 215 66 52 110 242 147 229 26 29 113 7)
      #t
      ())
    #(553
      "point with coordinate y = 1"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ANnLejLas0L4Y-2zQPPqYd34M-dVzma7GpGKQnFLoFvN9P8QmU9hap2AzQtIsybjqKKo9WNNgkh1tucft83de1AY")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 180 60 217 40 15 175 242 66 198 235 33 36 63 84 71 122 13 217 31 241 176 177 163 29 96 17 172 185 33 27 215 196 231 96 46 72 141 76 211 132 250 206 42 162 67 219 44 248 200 34 12 86 109 207 149 17 254 173 232 252 38 176 123 29 115)
      #t
      ())
    #(554
      "point with coordinate y = 1"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AHA93iAuoD0dZzc1ACzGLMdAU2EE2B_J_Y69t9-pCPWZ2P6kbevBkKWy719Ek_m17NjalAe_T8jhcygDp07mX3R7")
        ("y"
         .
         "AXybA42Gr8lBQD-sqh4qY3bewHXANassH0LbX82jrT_sZ7zyK69sgbQkG0qSV_jCEmiA4dammj5ax-mHEPsk1QXf"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 178 72 219 216 223 166 103 161 10 179 42 246 143 168 150 124 105 73 110 191 128 193 31 208 239 183 105 234 147 248 79 90 41 104 183 237 129 178 253 154 169 19 172 206 199 1 221 206 13 31 139 67 177 198 113 245 71 130 47 121 110 251 18 213 89)
      #t
      ())
    #(555
      "point with coordinate y = 1"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAT6vlPmMZNXHURSHTbEtkbimbOQ7-UOX6GnOOcAWG_kG_VDsH_k-vy3JDASRujAlsSZuKXQYyM6p0jbnSFj0QAE")
        ("y"
         .
         "AJKKWfPkvsBGTwIcWtCGRWIxpORPFi_mrvp6LK75AxuoN2i1R2LvkLHlCO3b72nlPz-a4hXUoGEvc30WCW3doyKq"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 8 28 90 50 137 53 77 178 10 146 159 163 217 96 124 42 197 140 171 123 31 251 40 2 230 167 120 175 27 121 202 122 104 199 188 27 211 122 5 119 46 248 194 143 70 9 85 127 67 56 123 39 31 181 162 116 174 62 136 20 199 80 84 68 196)
      #t
      ())
    #(556
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ALEItsseBDQUc2RsgPjJxRAUzsfwefloEQqzXA8Fsk6nciMntetbynSMNcdxq6Z7IyyCDtVE-dTv1D037EmWDbLX")
        ("y"
         .
         "AKx1ih4iXj2xmh8-qVg62epumUVo9t3BJLjatJuug1ezxwU3QE7U0CNw4WN-1ZFrQzNIWezj2-bdJgZcJN8RuPKB"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 14 100 166 221 135 97 11 251 153 161 52 3 155 81 140 140 200 112 25 237 90 47 160 179 249 139 232 251 59 45 144 3 129 165 7 85 115 155 255 72 62 64 14 90 217 44 1 110 227 23 77 248 213 40 253 192 138 23 109 96 128 193 131 224 148)
      #t
      ())
    #(557
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ASRQixgtzOpqziE25Totm2FJ3QrkUIMGZvCpyESRjw-z64f38_dwet3Xe7Es0eVS7xIQXGhnpP6BzR9qQAHD_m4I")
        ("y"
         .
         "AVdtYNB8ArJVnNGJq6pwPjbinGbT2hjTSSaughvSFpThUxkJPbJfYgtkgOBKTGxTsfo4j5WbZfv4qIKbOyYvVeTy"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 74 245 86 240 155 157 122 2 76 77 201 65 147 29 101 93 34 49 147 46 160 69 167 250 243 34 177 79 151 52 25 153 165 165 96 92 125 49 178 233 61 86 249 216 19 99 6 168 153 216 43 190 43 97 179 106 242 51 106 14 222 112 220 211 146)
      #t
      ())
    #(558
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOK10rzStIOHG32DwtsOlX7yI_ZbMPRdStozclNzeFwNZkqMosNbw1qVL9gisAcqlgxg4xn04G3mx4X9jdy76hjS")
        ("y"
         .
         "ADACNEcakqwsX3eMuoqX87PEXMjqwdgVtF3AL5t0B5rFZkkJPUNhMAWGezjz9mldUCc-ovaP-oEADHiV6R7FOFbj"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 132 119 31 16 137 27 109 171 149 54 99 220 65 96 11 183 178 188 208 204 107 211 25 121 91 200 156 226 204 161 136 60 249 47 214 250 123 22 251 57 195 115 116 84 189 60 34 244 251 44 46 134 145 77 180 24 182 83 169 224 220 227 190 94 191)
      #t
      ())
    #(559
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AK7g4wl0QdUBOIA_-bF4Br_KEGT-7CCf0LWuV8bYfZ5esyywZw8S-94Gw_PtixhhttGPa8aiVSJmokBobFKfBE9X")
        ("y"
         .
         "AQCJizxlAeuqgeae4Bm4edbuNxXVCW34OWHhC8gF1cZ0yqmKG6Ke0ASAiTGpYV3-2oZzzznwzkphjRgcBKhmGJA3"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 36 247 146 81 200 110 6 172 24 230 64 76 211 3 194 250 188 170 149 100 33 140 22 12 31 183 169 159 178 198 153 179 174 101 202 45 168 246 106 217 215 92 238 66 241 159 29 119 241 148 253 242 173 120 84 24 107 151 123 162 108 14 135 205 171)
      #t
      ())
    #(560
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AEetJko3PEmU-UKtCULa6LI6Mp-3pG6YfLkhwovK7UrtYOXqb8ztSz7OSrfZGzSePkWrzpPpdl7i_p8T1fIwcVoj")
        ("y"
         .
         "AFllQBClfw6dHtRq-sSq9bMih2PtLLK014oTFja9EzPxJ5l3mp8P7D7yRFLFHo5PMe9twRKaRU8HkljqwQrfiHmv"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 100 110 110 217 92 71 246 8 81 177 107 141 161 33 183 160 39 174 155 5 250 203 8 191 82 239 23 230 122 14 251 28 73 239 144 61 232 144 130 211 179 74 202 204 92 251 99 235 106 98 13 14 20 152 114 14 4 85 158 228 118 187 154 117 213)
      #t
      ())
    #(561
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAddqmbItCmKiozTlQIr8vJ-UoMIaLE7F2DyK8KcmWZvOSOFgFuUkOykdt79998Na0kYHDcjdwkzyCdhovfTzqOa")
        ("y"
         .
         "Ab2Jwv-7-0YfIS0W6akEclPNzfF5znY_1JFy-L_uto0cH9bi_W5kFqiwFVEyInNDYPUSgM71857F_9x1bkSllCdk"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 41 233 44 139 207 94 115 87 50 167 248 118 50 26 235 38 85 232 210 38 194 40 229 29 90 182 252 124 5 182 60 169 64 10 12 37 191 161 43 10 126 188 132 239 214 47 105 93 167 205 68 193 206 209 237 92 183 136 196 222 18 165 23 241 183)
      #t
      ())
    #(562
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AEq82EafGU0MzA20ZJLN37BVK8EwYrdIesw49ZoHT2ggAdsa3e9rKMVHnq9da5WzfDlOuR7P4C8Ah_xjlwC0kO3f")
        ("y"
         .
         "ASE5hnOvDWORkeMSLex7WFJt9wVGJ_aWpWmJKFH5BDgtv_e2HtSrGVORDSeqNWCVospFlWRWyNThrFax_-yV--jP"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 206 224 50 142 117 240 22 208 86 152 107 76 127 206 238 118 16 35 127 105 220 44 187 182 38 102 89 83 85 65 38 159 133 30 54 224 136 141 99 91 80 107 140 0 168 222 231 217 135 116 93 141 6 81 157 21 188 117 43 213 117 111 227 39 170)
      #t
      ())
    #(563
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Ab0WOYnEB9X4-f0uCHsUc3EMS8LWqX0oGYTBLLsGFb6ayAbCksnZDPNf_sZldgGTsddoHEf4va43rFDYpAdgoEf8")
        ("y"
         .
         "AWfM5NxU5n6_VkB67DOlqiC6hnyFb5KfrXeLObDcUSSdJOOQ1-M8cjgsSh0Cv3PWBZSKc6SBvfMp73t_BM86Mzx2"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 30 135 42 83 75 208 121 85 13 80 162 222 241 212 57 8 59 179 66 58 88 169 35 222 210 237 125 231 104 2 41 129 183 207 199 92 151 12 170 44 187 242 196 199 207 217 207 152 70 132 78 156 84 0 53 103 0 8 10 112 151 240 14 21 72)
      #t
      ())
    #(564
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AfPWWTeJl6deRW93DzS_rGOPHTh3fNCncgfLxCtUDB_LrVg7k4cxY0JuuRaZ-MCDSxNpVADeSXlniMWSQQxSDoWf")
        ("y"
         .
         "AZRiJcceJBo6eF0m0SHVYUXqj5do86AJouPFT5yHa4megasSYbK9XvmfVPRFNfnMdr8PROkaXxOemSe4jRbczEs5"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 249 113 126 138 113 207 169 75 148 61 65 191 18 199 11 207 81 67 116 25 178 218 11 110 22 13 6 163 24 125 120 23 105 199 93 42 159 151 168 73 158 57 96 105 160 89 114 95 71 161 1 3 183 142 86 142 7 104 73 145 18 242 241 107 121)
      #t
      ())
    #(565
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAUg1Pk60VvYOZaX2liyA6WBo4aMVcitmvmu9hPiFARuVvKoI4L9nq-x9Sgeb26fC8u0OG_9jN9dzgklfK2rl6AQ")
        ("y"
         .
         "ARtCWFPM95h95yRZa-DCPx1eHH0JMtL-csXyI7nQOni4jLCeumxCRSQPhUnHIW5TqHnJZmjN3AHVHHpS2yAat3uJ"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 145 192 86 95 221 86 18 152 117 86 32 217 167 105 60 252 225 133 197 123 99 145 228 249 185 15 123 34 40 139 79 62 163 217 93 212 217 27 115 22 21 100 106 169 207 102 219 138 160 195 2 255 7 3 2 108 48 58 140 59 31 62 21 236 173)
      #t
      ())
    #(566
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AFaKeM5dLQMP-FCHnuA8IB5NxkxYWI0sj-s7qbLQOFevPCnPvXibeX-Nukt0cPDIQSEjHzVrY-YT0P_1-KrqjIb5")
        ("y"
         .
         "AD1n3Qu7HYWI8YzPMcvlzShkIrcIw4bB-BAIZHyCT2lMEVNVMAl3PGV7K4TNrpjlL7YkCHKjG_sP7j8tipTlMZ5J"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 248 107 53 3 138 249 55 26 132 18 215 94 93 70 164 250 118 237 169 215 190 116 11 20 232 207 25 65 111 213 223 42 76 237 48 100 252 56 70 174 153 84 36 136 214 158 56 121 97 159 169 7 140 185 135 221 13 20 134 11 114 77 169 51 159)
      #t
      ())
    #(567
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ANck7QPOqAxXlMDwd7gGBmLddE5OjUGQ9_MTxAWX_pS9KqcLIPc6xNzvmTE2CMIDHHPxPtH12azoN8WA_QKk8tbV")
        ("y"
         .
         "AVX_VW8EZlfuX1B1e5B4xUZ_7IoPdWasFdsWiv7Lf1FFQdLR_4fF9nxRGk9hqRxXmZFImtWhyMDFVNmzbRSGsn5y"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 209 4 123 247 230 34 205 66 142 28 118 170 44 4 74 32 88 162 217 244 181 193 121 207 156 157 70 7 0 129 129 177 89 193 86 71 62 125 37 254 73 221 99 173 21 0 115 203 194 222 47 169 202 213 13 186 217 160 141 86 235 34 216 211 65)
      #t
      ())
    #(568
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJM57xwfk8pf7ewf9f4wM06xI8MOahfHtl86yEYft3kHX-aYiaQoN7Aer0S7f_iYTAvtoOG1J4piwH7BKMqvUti9")
        ("y"
         .
         "ABnAlf-tae2ADiI6i_pV0h9Yjcf59BudddwBB5K2_iQ9IwjxLfzTEnKevvnMQHRDwEsM2-V3FGFSQdq4dFz2SHvJ"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 62 239 83 82 117 225 95 119 158 193 62 92 199 255 147 156 61 193 82 231 154 173 12 50 160 59 140 185 248 196 73 180 217 70 155 54 46 149 152 135 197 166 242 9 98 120 60 102 123 50 179 121 31 167 1 237 82 232 45 30 61 34 41 115 59)
      #t
      ())
    #(569
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AMJakGU3fS2ENyWAomh2Z83OzqWAvhpecuH9KttC69qNanCsBT5J2khXMr8TQwKC-kztrGThsGkwUdE1TY1u_OwS")
        ("y"
         .
         "AeLSv7e0T1NbhzEq5nvqPWKAC2nUNCL802iaG87Kb8_kOZt8MYpWiNC00jM5ItWVz4FCWE-omMaUJvS8R4V27ci_"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 144 52 137 106 250 104 31 220 251 17 238 12 3 229 197 136 166 82 57 190 142 85 90 137 40 11 122 152 20 23 66 84 253 180 150 236 15 174 178 161 214 129 109 39 106 206 170 152 165 62 9 239 232 74 117 44 9 197 1 178 186 202 186 105 184)
      #t
      ())
    #(570
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AcJALzQbThyXVbPhAiI8gFY8YcQaZMsRmVjEG-wt2VGaR1q4S6r7cIOXAAuAudJ1wT1820kSfh6ymmXS03SQTQkL")
        ("y"
         .
         "AZ4Gw9pOiE1u2TXEuSyEvGPpHL1mXO6GzBUTRKOjY_p1rVapuAS0ZR7YVmMRQYjmPOuywYh_lplNuXA8HQZDgSzW"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 40 101 85 27 169 95 204 219 12 129 75 20 91 90 87 200 8 180 125 91 8 37 158 228 17 116 119 10 115 2 215 74 44 199 140 120 34 166 237 250 25 178 46 138 13 97 98 96 241 133 30 13 93 186 80 169 24 142 69 175 203 125 187 171 64)
      #t
      ())
    #(571
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AIP5nPJ2fODMiJjkYfkQQGCpyeoTQqKv9wVhBGkoZZKqQfMZxQyDUkiB_EIWnQclUIJc-S5a0RCr136OzdjKCfle")
        ("y"
         .
         "ADevaiTW28nIXfqtpsWpRd1jSbnx0e4Q8l-N_PBSLzYjM3v8uiBOfo4IwlVktP6TPLb8FF_--CuqL9A6iWeP-3WF"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 179 132 180 132 5 57 111 220 178 197 127 104 11 229 47 145 183 200 133 34 94 207 180 247 134 206 108 20 144 22 43 60 21 235 216 230 117 94 70 154 127 252 185 28 253 116 6 207 10 185 52 164 100 92 58 3 153 65 231 216 205 192 125 218 206)
      #t
      ())
    #(572
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ANf7TVWS589yTlfmq5XhWNgY8Zf3QDExjcg9PCvsXNhIZIH6yXrWtIHoN6u_NSuZLCJksW9WP4RCUmvG3QWmN034")
        ("y"
         .
         "AWHdkMkI9VJO5rFXqG9nNKJeFAY4vdg5J2-wmz-tk-e6iZ1raz7ST_-NSZrZjMRaNdYrjEYffMJWmXI6Az5bHbsD"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 139 0 182 92 189 114 165 28 39 157 27 89 98 141 78 122 26 132 114 119 246 216 189 83 17 229 234 233 69 200 135 228 200 2 77 196 18 235 66 5 199 97 3 234 73 62 37 223 76 223 0 17 97 158 62 251 41 13 28 234 140 41 5 129 165)
      #t
      ())
    #(573
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AFhJkxW7IpfXj45002NLv4O9wQ6uMG2CYK0MYqQcv3iSkUS7tpNxrJIY0Y7uWf6CAMEBc9OAzXhDgI6xayxM3Fbc")
        ("y"
         .
         "AE7N7K_lm0ru0ITS1zrwkRp9VEdIdGib1vdxbBbuX08pP3K6fSaYnlUa2sBWjNNFxclIIx1eSbwmKQEVGGsYW13t"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 125 245 63 141 50 161 234 31 87 52 181 148 86 86 76 172 70 31 28 58 148 157 79 237 255 94 169 105 40 231 242 180 117 53 32 51 71 96 241 78 222 21 234 169 100 246 160 229 32 52 108 79 107 59 64 27 166 227 1 181 129 221 73 189 33)
      #t
      ())
    #(574
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AR0mYLD4-lu6rrDh2eKpZCVYlp8TzQjFHFclsZ8YSBryvm7fC8LOWQZbvtSx84PzL1L8VZF08qQCJ17pUa3qAAkd")
        ("y"
         .
         "AFo0JpRebLcGuOQgEKaaR8CZSP7D8sVfjaXO9ey_Pw6j3Czeksvi7rdtx_0zglQ17lRXvremBp4-NI0m7TPgfc24"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 114 94 123 91 143 13 126 178 212 252 96 69 214 27 91 133 5 161 86 254 248 250 75 26 46 53 23 96 178 246 53 161 131 127 35 164 198 183 36 223 7 107 33 135 183 226 19 218 99 107 6 37 12 102 25 58 35 90 78 60 144 237 134 235 91)
      #t
      ())
    #(575
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJyTBl1nzYORhyM_v7M_gI96bNREkk7k6g_Uu5s9vItK_6fTyOOsKr-CRJ189BZsbBgbT2Cf4J4_96K2ZA-BY7CB")
        ("y"
         .
         "ALX0SpR7fZvHgGnRXVBJrB7p2iHngTFSPrpNnJ7mcm0yvUrYSdvXEnBxDMyzy4jSQxiP0ErDlMEl3LYnNVcOEjiQ"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 113 142 144 10 126 249 147 214 113 171 13 197 65 124 152 246 31 110 176 115 22 65 229 82 255 72 250 79 147 49 139 139 255 250 255 42 28 250 75 117 157 53 30 86 3 106 97 221 16 97 216 92 61 20 75 138 136 47 70 152 16 197 142 6 70)
      #t
      ())
    #(576
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ADwfG-Rh089DAs8fqOuSHlwoBv5Wc_J_baOlvCs9eKiq57pBAjbdnmUJQszbEQQjq_U8XRMWdjj_gWK0uTGgvLup")
        ("y"
         .
         "AGCNZRcAnZn8O_EWXoGZIhp5ifLr2jtzmnSMyTjS2z5pfF91wy3VvMVXjlSeQp9FSgOVeeoeeWhR51eO_SgIWMyW"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 97 200 76 100 203 214 94 113 138 191 171 31 130 122 243 48 100 231 2 154 230 254 235 17 224 87 24 27 75 127 243 219 206 77 133 104 98 180 154 187 192 185 35 22 67 97 114 143 98 116 216 144 96 59 124 225 135 215 204 252 63 241 87 180 42)
      #t
      ())
    #(577
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AQt7b62h9gLJ6X2krNfr_S-fjjDgswt3-zlU3qr6oq4mKLoEK5JXoLYR33cKetcNp7miC_TW-AUSzudLohTGHG1m")
        ("y"
         .
         "ACBh-JDNgbm0nJEAejJd22fEYGK7ZCZtPnKTQknLcmK4uSpzcjT26Ft0-Bi5VKu8NSnaEWwDIfguId0l5TwHOrvh"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 114 189 178 74 199 239 116 26 148 193 90 204 126 32 141 141 1 24 59 225 196 95 94 81 13 161 192 128 145 2 169 96 63 227 13 203 176 51 37 8 106 237 148 186 35 66 43 233 64 66 135 205 234 228 162 136 175 221 194 170 160 232 203 196 21)
      #t
      ())
    #(578
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AR0osalfvyekrSHvRhPhmDzONUwBiqdXMw_hNXL4SKv6JViY0TmPE86waztTKS0ADofdIN0ZVHY6lP3OsSj-Y68b")
        ("y"
         .
         "ARH_wbJoZtz_nkIQShIaJO9aFdeBQcyTwmFnOYl1kW6vyK2LquE_Wd8j_nmq-24geLpGXGsU5EwGDuqqGub0x8l5"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 36 234 207 213 152 182 252 32 185 240 24 115 237 60 85 176 219 179 249 219 193 138 5 35 62 89 164 236 114 112 208 18 183 144 132 13 34 5 28 79 28 85 162 82 232 173 177 13 174 133 85 153 226 42 188 179 226 57 4 170 7 103 227 204 132)
      #t
      ())
    #(579
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AYezERpxixgHfZeJEl69tv3sd4kIdUQNNmkuMBWbTPveGAez67GxqMfbAMH6Zs8yxfmuD1lB4y18fSrw-piDLwG6")
        ("y"
         .
         "AI997yBjp3l2JSNnl_vq-NB9dOB9E55tc1g9LUUKd5So9xL_eFKqoj2m7oFC2baXVo9_SmOof5fWNU_EaVlkBpJs"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 47 220 255 68 169 175 148 241 139 80 244 156 25 161 224 230 249 153 150 77 92 164 50 127 40 11 46 219 171 248 35 93 46 43 162 225 1 179 145 208 220 26 160 103 211 89 60 175 179 197 169 156 5 49 101 250 40 214 119 153 14 168 134 252 122)
      #t
      ())
    #(580
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJpkuoxmYrtRVT-FYIqi5SK7_9G0TvZG3gk49iZ5kN2df1JQUTa2diCvwfbSWs9tHcOXLDqISTrjknIwIlwDqBNd")
        ("y"
         .
         "AIlSwV3JRGGHPCMqL760p6RofmQdELM0jRejHqP78XztkGW_zd_ZKsdCzuYdW_WCuVjSU-k5zlz5LOeWFUM9Bp2N"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 205 243 240 200 19 186 223 116 84 222 82 13 18 72 178 152 80 33 148 80 74 253 89 4 8 9 65 203 133 163 113 86 40 72 228 117 96 17 113 53 147 175 70 20 93 6 82 178 237 206 24 213 201 228 164 205 85 55 11 205 96 131 153 6 40)
      #t
      ())
    #(581
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ADk72P48SfeIXvvKuHli7xJFjBjyVfAaJKGreVy_4rTd9uisJT5P2UhDd_Wd3HwIeZY4btmhZcyoH4U8kYvhydOZ")
        ("y"
         .
         "AHxHrKSu4XldQUxDNM5L_sr6vzwzkaAukzLX8X7BtcVCAJ-YD7rz6tzBkvQAua0kACIMCQp1h4Tbpm3kBgDGJuSh"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 209 8 37 187 113 208 148 234 43 1 133 85 131 168 34 104 113 235 231 214 188 174 6 66 39 189 216 65 120 141 28 72 155 148 219 32 195 149 98 69 13 184 71 53 249 40 82 89 179 76 93 59 67 164 135 240 112 134 50 183 93 226 235 225 4)
      #t
      ())
    #(582
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AbSxmhJbwMvQYGBkoFGvSrF_eRiAR18AofvNSnl7QBvaOQ73gm-gFoJlHXLLE1NwTXoYAnxNYJM0sujF1N74bqDk")
        ("y"
         .
         "AZDkzWpOC6nqRdO3FE90qvGYZGJVjIs6EFAYgsJmnsJl78FS9VEPPpkNZyGEccr8ZshFz2nl7_bgMIzFmsWaQHZn"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 49 235 20 102 22 215 188 97 255 181 83 183 202 133 34 165 182 8 141 35 213 202 110 48 178 1 184 109 162 55 93 31 108 196 142 136 162 75 82 176 226 32 69 202 124 80 108 32 105 48 97 156 40 159 196 166 136 67 42 222 228 112 40 128 211)
      #t
      ())
    #(583
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AOf6SsI2Wvk3jdLIHzL571XU00GsMk60aSPsfxVEjDfvYHyXXDUnVPRyhz5Gn403042X0WGnubHa4ull_cyVl0e6")
        ("y"
         .
         "AebAljlylJm2BzMNfauitcHvrk8SHlQL9-aRgJoFVUIoTLXuh0dVic7GY7U_IbkSMaY42EAuNY_RaZU0kas5UxIq"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 168 242 218 215 73 234 30 21 103 53 134 187 103 181 200 95 248 244 92 114 239 132 71 239 239 62 252 98 106 106 16 202 136 69 22 67 180 52 162 81 118 203 174 119 137 154 62 50 164 72 71 129 183 0 224 56 87 189 209 88 173 60 251 242 0)
      #t
      ())
    #(584
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AQT0P_uzfOXQ6pVhccBw1hR6z6lRm4h5lYMqDrgB4NvGaJbfr_lECbSX9KQWzNgnUNpHpSC4cN_mzTdk8OPA8bw7")
        ("y"
         .
         "AKXz25k5whFHq0SIyBKLoFZPGS8niFYBRdKxSoUoFrSFHvaaRiyYVLrOcg5GKe1Vr50sfadM0wn8VWbhFjB0ajUi"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 238 133 163 182 211 93 13 246 164 69 89 60 148 96 153 50 83 14 37 238 113 144 156 86 129 174 57 138 56 48 158 134 153 233 78 15 35 245 108 100 160 208 79 193 14 40 150 197 15 33 63 18 198 133 21 31 164 15 24 172 109 142 216 89 6)
      #t
      ())
    #(585
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ASUOytiU0_3_tYAJnCwwW9XZA_7dLlMupY0p1jwygbOeb0ehGzvpfBwE4ELT8ZvQy-DvQSGiVyebvXbGagXo0iLQ")
        ("y"
         .
         "AaKzzkHKCbW2KHtMMooxXcokPHJuhhZinSs3UGt-sx63b1E97LCrIKUsDCYKkg6Srhpin7gX31rW1-YjZ6xejuXw"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 30 125 82 157 201 97 82 9 22 36 1 171 43 183 23 55 129 9 216 122 26 200 107 152 183 149 78 169 27 150 11 8 250 147 116 234 114 112 53 25 195 156 105 121 114 64 201 119 129 126 43 52 181 50 38 101 221 81 130 81 188 193 250 151 11)
      #t
      ())
    #(586
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ABn2_9gdAStwgEwwhZS0cGZRa7szm0SrrbTaGUOZ0bTV_JjdnQ3DY6m8sUOWMn_62zn5s1e35jGoSyMDk5bxzJbx")
        ("y"
         .
         "AaaowK-OCDqfjBnLD7rntFI8TFT8n_77c1ET9Sj3o-lmJXEGCGHdbJxxsz2ZtT1xbDaoqQK8pkxGx-NKLojbyWtt"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 107 70 187 218 159 55 150 26 236 158 252 214 252 186 244 226 91 102 116 234 55 248 216 44 162 237 204 6 53 62 36 208 158 165 216 97 54 254 222 227 78 12 57 27 133 144 114 171 145 145 139 81 208 128 10 178 2 85 244 56 198 96 136 46 145)
      #t
      ())
    #(587
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AHfYl8_2oqNEb7OFKK2AzzzFA62T2gbHxaUlz5p7ujPXnWhqDFMpCkWUJAVAoCqFzxQzaiPaVu_bVisGVoAP85ak")
        ("y"
         .
         "ASB3muDffB2SjKGNY_yr8NIb1DfIbkCWa66Mn-8qxyc4p1vkmTirmRcDSiU2wCNpXoIfcLRYM5vY3iGKXPdB5Vp9"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 16 153 137 182 42 55 147 151 35 44 35 140 245 125 129 136 75 171 179 155 4 31 191 15 11 72 187 125 121 73 68 249 163 2 222 14 204 170 154 86 37 178 34 198 105 45 36 7 118 84 241 152 54 184 241 77 134 34 186 20 238 172 226 116 201)
      #t
      ())
    #(588
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AaAnGSRN-AeGRwf7xsUoXaeEMB2r_7hYU9r5RvjXFZj6lffJCj1fBuRQUXmcFvmvCv4nX_T1HoPhv6vBgXbyvEp0")
        ("y"
         .
         "AVTemcdN0mNFYDSr31sqFz4xhELRMgD0CQqqJxRSTEOoXsX2B9XzVSBf7w9Ko7QbL_eeyjYfwoh4_U4mvMV6wBTY"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 5 75 97 145 203 37 126 15 24 208 156 244 84 252 164 196 87 22 234 0 241 103 178 251 210 208 40 182 108 144 59 94 117 26 108 170 204 244 237 24 226 134 53 216 69 78 211 129 30 28 75 54 56 235 66 10 210 30 164 253 189 186 218 232 102)
      #t
      ())
    #(589
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AYqCy2CBL9NszLKeR0fSBXmsmsa172SQMmswxfB36deEHtGJrRchQAVdfuawFeArHrufwX1qzYu0wI0t20VBu4j0")
        ("y"
         .
         "AHdQ_iZA5ZDxVCbWTiM0yyrVlzQMRFpCN5_BlO901-BqIgqtuOaeNhtJYN3nDwhekkMZ3qDfHUwSxBUZU1cCZw6o"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(0 113 19 245 243 197 71 162 189 238 74 247 98 91 241 97 173 152 64 232 63 44 59 135 27 24 190 12 157 181 104 108 175 211 148 140 131 26 37 26 70 73 153 145 147 133 38 80 230 220 138 159 130 204 105 106 213 124 113 129 172 154 158 194 32 236)
      #t
      ())
    #(590
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACc7m_OBjBKcwnInC1uwuLflV8qMVIpvMgXje2EZYrFNm-MX3ET-grLSUEmE25ipAufqKlr-WE3CspkOWnkP1VsZ")
        ("y"
         .
         "ALkhH7-iIePCS8PeL3Do8OhPSwXC0rxemNo-N56Avs1T0iETatdAsnMuy8PH4GwH4GTV3mnmptVncT9kTorCWh2T"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 145 136 71 140 98 52 146 153 186 47 18 174 195 199 239 101 133 143 80 57 83 128 237 14 126 112 231 49 215 196 120 133 207 177 131 236 202 34 252 113 240 175 208 98 18 67 117 7 134 150 154 0 151 179 106 82 30 244 29 249 240 143 154 69 88)
      #t
      ())
    #(591
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ALnUA99bHay-8rrs6IoLEPq8XXKXU_TpNgFa-5a5KTkqnusDRglo7RiGhxTKrC3BbAckWp70gyrEGOMpDR-NDRAu")
        ("y"
         .
         "AISU9EQtAP3KZ9MbPrZWyKBr1SEEbBrwdbDtJsJtW92LgASTxoz0k1FgJ-3nca7j7YtfuUfp1gDMCoLD8gdtlqrA"))
      #(("crv" . "P-521")
        ("d"
         .
         "AXgdhsrCwFK35PSM70FcXBMZ4H23DbkqSXwqx2TpUJrAsHMigB9a4fKMnX23H3nl9Rv2RnkK-YjWIzmm0VQxkuMn")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ACbkvI49_LKJj5-3HrNzGZ7NWLE7ZsIhEQgZvQUVnrXjuNE3b31RUdsUCzza5pi8qWg_PQQRZP4OPboNLwLWZDrb")
        ("y"
         .
         "AQo_H8ttvMx3PRQ9RUNHFFqb7EmOel4tQSCVuTUMWNjpQR6oWv5MVgU6012L5CMPXicYNl9D29LtRLCWqQv8Yie2"))
      #vu8(1 173 247 36 207 71 174 49 151 145 163 219 227 54 178 245 74 77 116 188 40 255 138 35 194 243 51 226 165 163 107 190 115 221 108 157 231 38 22 173 91 119 158 81 99 109 8 247 206 197 143 241 139 113 39 227 150 133 107 193 61 57 9 68 16 175)
      #t
      ())
    #(592
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aac9NSRD3ikZXdkdamS1lZR5tSpuWxI9mrnlrXoRLXqN0a0_Fko6SDIFHaa9FrWf4huutJCGLDLqBaWRnS7eN619")
        ("y"
         .
         "AT6bA7l9-mLd2ZefhsbKuBTy8VV_qCqdAxfS-Ksfo1XO7C4t1M-NxXWwLVrO0d7DxwzxBcm8k6WQQl9YjKHuhsDl"))
      #vu8(0 242 36 100 49 181 151 147 15 46 174 97 233 170 187 211 159 143 106 233 124 60 242 82 26 106 238 206 221 161 11 94 245 243 178 235 58 137 6 208 47 81 210 68 113 10 169 225 156 192 190 33 219 146 1 50 190 28 145 222 184 94 70 108 40 223)
      #t
      ())
    #(593
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "AAAAAAH_________________________________________________________________________________")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "APDdai5T8j6eULgfJV05C1rMMcyEBulXUfsVPGv2Wvm7GFrVmwrgyzXtIbsHy0OpWx9Q2_7wdjnLSMP2rGRf1ehn")
        ("y"
         .
         "AR_SHVvpnCjon7W0xs4tcTkLMwaPLVBHZXX86DUY2cH94hZy-aLp_ll_Gv-rLtG_b-7x6AneeNl-pdkAKi9xZFX7"))
      #vu8(0 52 124 81 245 135 199 38 7 11 222 185 23 61 10 84 116 39 234 211 242 200 222 98 217 236 195 1 50 133 246 69 210 32 147 21 32 188 239 133 208 140 251 103 134 4 87 69 251 251 251 25 36 196 74 137 208 102 118 19 26 150 86 119 39 42)
      #t
      ())
    #(594
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "AAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AeNeGCRe09rZZdLcZ7xu4DCJEReEExQgX0YWxvgOgNOGgVYZRnXzjFJDuZpdyBweOLGh_AMyvpYUC67XZmrmsoU_")
        ("y"
         .
         "AcNUGqW0ZAwsYLQ-T9dwCKNWuQJ85Hz2iJYVrDFFboETE_OhoIIHZAzq7BXSeiYLMrDzQdZJJZ5NyzieWcr9p3Qe"))
      #vu8(1 196 29 196 67 124 47 43 148 169 64 113 27 58 105 23 35 57 122 31 131 214 188 12 103 221 199 166 87 22 9 37 199 248 91 180 235 56 66 182 11 38 16 221 183 192 184 103 98 103 113 14 88 53 154 135 80 132 60 109 142 37 212 141 28 217)
      #t
      ())
    #(595
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "AP______________________________________________________________________________________")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AUqNTX6GVc3DP_fBRSjpEaOMoLkaccqkvl-IAJ2_DDk5FsOaypEJhJm99P1MEMb8brgGGTLTipMRQIblvqb5dAr-")
        ("y"
         .
         "AKZeU-K6t07lFmH0dg_u3mhFGV_5t66OCKjH_03cnGX4SqMmayvE-l-L0EJGzjEihDGYbPx7v8i81u9ZRf4hzzwk"))
      #vu8(0 106 35 156 219 122 120 56 64 101 141 95 49 75 254 92 81 232 6 164 191 18 54 248 66 18 101 188 197 3 198 115 235 22 197 194 179 139 87 23 250 4 238 125 188 222 177 92 135 23 17 80 122 187 117 87 168 168 199 179 37 1 65 232 84 213)
      #t
      ())
    #(596
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "AQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AC2n2wKEDwI6NuH__q7hbTxHu0Nb7GojHUqrHsVBL1b7kPzE6quf2FcQhMudolJGbAUtIZE84P2kfmGCmXLOj5oX")
        ("y"
         .
         "AJga86YUVduK6TFhosBarsIIwawwVCsjv4cTZeRKTrCcimJl-cuWorYyz3_fd79t_FnNInixo2YRmXJZ5xk413qe"))
      #vu8(1 18 219 249 113 58 173 212 120 228 242 235 203 5 143 5 181 18 177 149 156 125 161 153 79 133 31 55 60 232 195 65 211 156 104 67 55 63 111 229 89 144 89 83 225 20 118 64 21 148 55 149 60 87 25 97 192 155 173 21 122 142 26 91 244 118)
      #t
      ())
    #(597
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "Af__________________________________________-lGGh4O_L5Zrf8wBSPcJpdA7tcm4iZxHrbtvtx6ROGQJ")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AYUaVQnahvW68KGb7M_Ja_z6KtS57BMnUHW1DTSwj1jzryRNsSAKf88tTQyyWeYpQpEtjuFmAmECVpShguJonb48")
        ("y"
         .
         "AJDRMoEwhWyunKw1eTNMtvAK7SSuLy_HTn11xwzwWU8mZsxczbHef_A03NtQ89n5f2U3lIjegtjOzxAeuWOlP7wZ"))
      #vu8(0 62 202 34 16 200 98 49 5 8 90 162 132 209 25 243 215 22 115 5 149 198 41 26 168 155 243 42 149 232 165 253 198 79 61 118 233 36 148 164 58 157 206 209 45 5 182 220 164 255 230 73 179 42 193 44 176 32 46 112 45 200 58 44 178 119)
      #t
      ())
    #(598
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "Af__________________________________________-lGGh4O_L5Zrf8wBSPcJpdA7tcm4iZxHrrtftx6ROGQJ")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ALqKZb7o3i2ljrvsepGdFjEXX1GM_PC7DFFwsB-0vsrDC07zQbBhvfIkSCbgFaTG2GCsN7NodvNzobtZ8mdcNGWI")
        ("y"
         .
         "Ab1AtKB_khcDEjeYDgVx5BG_dZX916qEmSE3cFqe_ltlkArpYKPJgd6qiAsdgo1mBD4XRfF-UEJ7yWmjUHGpn2dK"))
      #vu8(1 196 202 233 251 253 212 93 229 29 133 37 232 68 122 117 83 195 92 243 88 241 52 111 29 121 102 104 135 187 116 154 59 160 222 98 225 134 107 71 164 71 213 59 111 28 165 163 62 201 69 7 226 207 182 85 68 245 161 25 95 198 180 220 88 16)
      #t
      ())
    #(599
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "Af__________________________________________-lGGh4O_L5Zrf8wBSPcJpdA7tcm4iZxHrrtntx6ROGQJ")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AaIcLgd-4xwG1W1n24wbbhZBnGeeJhF7pirlxheYPf7CH8JuOjz164M3frpxS7te4M57e-2H1mzBRHutIR1MqUzj")
        ("y"
         .
         "ASoR5Kw3VmLPbLHeFv9LWrZvaWgM1r4Tz_b8O5lHN0AdeGWP54xB2iUqReDMpiPTFJP06k649sjk1j8o7YYyPSij"))
      #vu8(0 128 115 180 121 110 116 143 61 13 229 232 91 34 174 212 99 241 166 174 205 179 54 188 40 123 80 209 57 227 89 30 245 248 107 120 195 246 5 20 103 117 95 5 159 41 93 117 128 117 52 125 101 122 170 224 35 131 131 139 185 96 113 234 203 212)
      #t
      ())
    #(600
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "Af__________________________________________-lGGh4O_L5Zrf8wBSPcJpdA7tcm4iZxHrrtvtx2ROGQJ")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AI7ZWmst1vLbso89xljwX5sn5C8QDby9XT6PR2FcqxNsro70r0Tw2fnxv7pluDeGsDOt55F6exLe9Kvxf-sAjHpE")
        ("y"
         .
         "AVryjQRQ6LIJOphhnrGskuzUE7CrfZsW_syZfZhPAXE734mAEb0oCbtfcOnGO0sSYe_qjnlmL-yhSdxE-Vq5RDvt"))
      #vu8(1 241 31 248 152 55 146 212 167 144 208 222 75 86 208 120 185 3 58 214 49 138 68 14 129 25 52 41 55 204 72 163 147 117 21 10 178 207 152 39 59 15 227 93 90 58 245 216 67 34 166 133 232 159 44 179 120 169 155 155 123 172 135 228 73 82)
      #t
      ())
    #(601
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "Af__________________________________________-lGGh4O_L5Zrf8wBSPcJpdA7tcm4iZxHrrtvtx6ROGMb")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AORUXIwOE0cAUmYidlO4YY5G7dqnuhexE-BtP6iV_-AZHIHuXtvmzyWKaa9DrZ4GIEYJULQujtP74QQpCp6GarZt")
        ("y"
         .
         "AKwhf2l4KU43C9CUSebeXRnqjpm-8bkZZMKfbcgYaHbYKD2nRQCZMFouLm60ymDq6mLmxBNNCN12uwc9BexJ0H4l"))
      #vu8(0 40 108 239 170 243 140 164 198 101 126 185 177 135 216 97 77 81 119 95 215 28 26 121 180 192 239 26 13 76 231 43 111 91 43 200 84 164 231 130 131 83 9 66 163 244 253 42 133 134 213 234 81 81 60 137 211 210 157 229 222 6 50 30 17 142)
      #t
      ("AddSubChain"))
    #(602
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "Af__________________________________________-lGGh4O_L5Zrf8wBSPcJpdA7tcm4iZxHrrtvtx6ROGOb")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AEfkzw8PVNDzLrF5Y7uuIIp20y4xYTtkeanJmFwkuRaGFvQCp2p_Zu3ARp3p_ZbFXb4z5vJEoRtElVxiL-7ugGRV")
        ("y"
         .
         "ARWqWC8daiLpeqQUt8cVQBFRGTW8s7hwRarFExkdEBrx-vYctN_s9vAHkocJYfb8kZKgsOyz6FAjNGjKe00AVva-"))
      #vu8(1 71 144 222 20 196 129 241 51 111 203 125 51 168 191 142 35 235 89 76 196 134 8 233 237 254 14 50 110 16 107 103 231 234 163 240 78 201 152 85 153 23 143 99 42 94 230 65 158 17 33 112 96 233 252 213 149 138 67 136 43 248 205 59 230 186)
      #t
      ("AddSubChain"))
    #(603
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "Af__________________________________________-lGGh4O_L5Zrf8wBSPcJpdA7tcm4iZxHrrtvtx6ROGPb")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ALvbFi2ChKkQAhmY4tFPM6jFGpz7mmmo_FcJ91Oq8cTSSKbdtWJlQPgdB8CSMdLsFUn2crbSvVfSL2TyroCT5zil")
        ("y"
         .
         "AMR5Dypt7ekQH-AWODVD70_1R-NgQmmL62LRMb20h5CTyNXnzu7Qq-zSgsUL0jcygehIE39PYFpg8b0-vXamntpl"))
      #vu8(1 174 119 93 188 64 150 163 174 167 151 123 26 10 244 178 131 14 207 156 169 39 166 36 127 186 76 204 180 107 63 113 208 231 171 184 221 167 45 28 30 231 187 91 135 91 71 115 204 141 244 15 115 40 25 196 20 125 163 48 119 93 23 66 234 53)
      #t
      ("AddSubChain"))
    #(604
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "Af__________________________________________-lGGh4O_L5Zrf8wBSPcJpdA7tcm4iZxHrrtvtx6ROGP7")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AYdbx9xVGxtlqeG4zPqvhN7RlYtAFJQRai_U-wur4LMZmXT8Bsi4lyIted8-S3vHRKpnZ_a4Eu-_XSyeaC3TQy10")
        ("y"
         .
         "AaNbbcqKJTSkLSmdb0RUS0IEe4_t1HGufZX3uDFkeSgSnS-Ifk5LDKez7hdkDi7MI_KklvCsV4N7Qb6ZYHrY_yq1"))
      #vu8(1 151 159 176 94 6 138 18 163 242 12 253 251 158 174 233 242 43 53 110 220 199 101 83 131 237 56 18 75 134 129 79 134 166 242 33 106 52 243 252 34 153 212 3 238 66 64 143 149 208 140 92 108 209 29 183 44 191 41 154 74 60 37 69 190 37)
      #t
      ("AddSubChain"))
    #(605
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "Af__________________________________________-lGGh4O_L5Zrf8wBSPcJpdA7tcm4iZxHrrtvtx6ROGQD")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Ae5FadbNtZIZUy7_NPlEgNGVYj0wl3_XHPOYFQat5KsBUl-8yhYVP3OU4HJ6I5UxvowvZulWV_OAriNzG-33kga5")
        ("y"
         .
         "ACH9qlLzObCnlR0i2Pq5HE7u1VREjCWlf3GNv1bZ3-V1aTVI0vGpm3NiBpNnsh2LDd_COEdKo18lIeFTMoenK7Do"))
      #vu8(1 151 235 226 103 152 191 103 240 111 240 40 39 115 175 117 17 85 49 244 29 148 192 147 216 116 129 183 107 239 112 123 194 34 242 214 103 47 132 160 15 162 12 94 210 112 39 171 64 6 182 141 147 238 33 81 1 108 157 219 224 20 52 98 114 226)
      #t
      ("AddSubChain"))
    #(606
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "Af__________________________________________-lGGh4O_L5Zrf8wBSPcJpdA7tcm4iZxHrrtvtx6ROGQG")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aac9NSRD3ikZXdkdamS1lZR5tSpuWxI9mrnlrXoRLXqN0a0_Fko6SDIFHaa9FrWf4huutJCGLDLqBaWRnS7eN619")
        ("y"
         .
         "AMFk_EaCBZ0iJmhgeTk1R-sNDqqAV9Vi_OgtB1TgXKoxE9HSKzByOopP0qUxLiE8OPMO-jZDbFpvvaCnc14ReT8a"))
      #vu8(0 242 36 100 49 181 151 147 15 46 174 97 233 170 187 211 159 143 106 233 124 60 242 82 26 106 238 206 221 161 11 94 245 243 178 235 58 137 6 208 47 81 210 68 113 10 169 225 156 192 190 33 219 146 1 50 190 28 145 222 184 94 70 108 40 223)
      #t
      ())
    #(607
      "edge case private key"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aa1QQ1kdvoFlf-PRw9elFmBq2dMgo1_Oiq7IqVD7U_lTiPP8SL6ZjpkzStnpI0ze0URx_obKzKoH0Fjuh3FzOsO5")
        ("y"
         .
         "AIVN42NmWQue5NA3DqawD369gVbM8U6Z8aU0SptJZPu4NIsIGohAxrZL53mXrYvr_qXn2femp_ptdlXFCyt4NfMU"))
      #(("crv" . "P-521")
        ("d"
         .
         "Af__________________________________________-lGGh4O_L5Zrf8wBSPcJpdA7tcm4iZxHrrtvtx6ROGQH")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AEM8IZAkJ35-aC_LKIFIwoJ0dAMnmxzMBjUsblUF12m-l7OyBNpu9VUHqhBKOjXFr0HPL6Nk1g_ZZ_Q-OTO6bXg9")
        ("y"
         .
         "AQtEczgHkk2Y_1gMExERLA9KOUrvg7JWiL9U3l1m-TvSREwciCFg2uCUbGyAVmXNtwsVA0FqEj8LCOQcqSmeC-T9"))
      #vu8(1 193 104 49 76 220 133 117 122 222 52 165 42 158 83 121 255 165 150 143 8 75 126 64 73 57 168 3 58 15 198 152 226 98 17 117 75 155 44 4 207 138 20 32 171 230 233 134 239 26 35 139 187 145 221 64 43 114 224 237 80 168 118 241 168 62)
      #t
      ("AddSubChain"))
    #(608
      "CVE-2017-10176: Issue with elliptic curve addition"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AMaFjga3BATpzZ4-y2YjlbRCnGSBOQU_tSH4KK9ga009uqFLXnfv51ko_h3BJ6L_qN4zSLPBhWpCm_l-fjHC5b1m")
        ("y"
         .
         "ARg5KWp4mjvABFyKX7QsfRvZmPVESVebRGgXr70XJz5mLJfucple9CZAxVC5AT-tB2E1PHCGonLCQIi-lHaf0WZQ"))
      #(("crv" . "P-521")
        ("d"
         .
         "Af__________________________________________-lGGh4O_L5Zrf8wBSPcJpdA7tcm4iZxHrrtvtx6ROGP3")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AbwzQl5yoSd56ssu3MW2PRKB9-htvHv5mnq9DP42feRmbW7buFJb_-UiLwcCwwlt7AiEzlcvWhXEI_30TQHdmcYd")
        ("y"
         .
         "APL5FmZ3pJysohwYsswmGcL9sE-DHy5pDarTcbX_U3s_u9y1FN_ghW7MbqLktLrfZGJYYB6k5gewLsonvh0nBleV"))
      #vu8(1 188 51 66 94 114 161 39 121 234 203 46 220 197 182 61 18 129 247 232 109 188 123 249 154 122 189 12 254 54 125 228 102 109 110 219 184 82 91 255 229 34 47 7 2 195 9 109 236 8 132 206 87 47 90 21 196 35 253 244 77 1 221 153 198 29)
      #t
      ("CVE_2017_10176"))
    #(609
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(610
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(611
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "Af_____________________________________________________________________________________-"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(612
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "Af______________________________________________________________________________________"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(613
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(614
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(615
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB")
        ("y"
         .
         "Af_____________________________________________________________________________________-"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(616
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB")
        ("y"
         .
         "Af______________________________________________________________________________________"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(617
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af_____________________________________________________________________________________-")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(618
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af_____________________________________________________________________________________-")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(619
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af_____________________________________________________________________________________-")
        ("y"
         .
         "Af_____________________________________________________________________________________-"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(620
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af_____________________________________________________________________________________-")
        ("y"
         .
         "Af______________________________________________________________________________________"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(621
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af______________________________________________________________________________________")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(622
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af______________________________________________________________________________________")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(623
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af______________________________________________________________________________________")
        ("y"
         .
         "Af_____________________________________________________________________________________-"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(624
      "point is not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Af______________________________________________________________________________________")
        ("y"
         .
         "Af______________________________________________________________________________________"))
      #(("crv" . "P-521")
        ("d"
         .
         "AcbK-3TipQyCx6Y9EylL_qET8nHgGuMF95r0MgPNMhFezfL-5f7boq0xJng9sMPE0wKaFDaej4Db0V1RLxPlHFA8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJsV2VQvRXnYbPaNO2CsdCT7kCvhO6ZASWpAt6SsTQV7mQrWfDQlQQg_N3r2MiSOa4dt8d5cmFRzFQ0DBIsrzb35")
        ("y"
         .
         "AF7A0BtEJCH1SD4BbPQvispAEfAbn1RaiyDgBWh4UFBKqCgf3QCqMoc50Fome3qa_T9La5sViNTvEOH4ZzZSThtX"))
      #vu8()
      #f
      ())
    #(625
      "public point not on curve"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AMKkOsN0KwngBsLfycNkRNfmmfVnpz9nTOJXMwsxLdeooE--ktHZrLzGX2GEcRraW_OfbhHgy96Y8WQNCZ65Dfzn")
        ("y"
         .
         "AYAtd1XC_jGAhI1KcLFwCW7GTrqZtHi6f2_BKbBWYnm44at5YvqRL8SuU7UgKgNSBheEPcY-XLX5Vux_FFPQhl_h"))
      #(("crv" . "P-521")
        ("d"
         .
         "ATlqmaM3gh2MktdfVieTxwr6QHSuXm2tK9LMauqPNvbEXd3nOTFEDSKfNACTq4xvs_INIJmaNzcf6SEEaSE20Bm3")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aep4GB4E9ID9mJ4FlkJEYTt_emN5q_CBkyep9a3vpvRBB0UCi_GkaoAx_scAN-vLUH29j1C4EqS8wDE7p98Cx9NV")
        ("y"
         .
         "AeBFX-bOHqmYxzl1aUjPonu4sangygpQJjGDb5bg9d9mnLPKP298D5JAs9h-F0Fhs7BNg5q2_kbyRb1iZQXvS0FU"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(626
      "public point = (0,0)"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-521")
        ("d"
         .
         "ATlqmaM3gh2MktdfVieTxwr6QHSuXm2tK9LMauqPNvbEXd3nOTFEDSKfNACTq4xvs_INIJmaNzcf6SEEaSE20Bm3")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aep4GB4E9ID9mJ4FlkJEYTt_emN5q_CBkyep9a3vpvRBB0UCi_GkaoAx_scAN-vLUH29j1C4EqS8wDE7p98Cx9NV")
        ("y"
         .
         "AeBFX-bOHqmYxzl1aUjPonu4sangygpQJjGDb5bg9d9mnLPKP298D5JAs9h-F0Fhs7BNg5q2_kbyRb1iZQXvS0FU"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(627
      "using secp256r1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y_ZgZZWj7lD5_OqieYwnQMglQFFrTlp9Nh_yTp3RU2Q")
        ("y"
         .
         "5UCLLmefnVMQ0faJOzbOFrSlB1CRdfy1KupTt4FVazk"))
      #(("crv" . "P-521")
        ("d"
         .
         "ATlqmaM3gh2MktdfVieTxwr6QHSuXm2tK9LMauqPNvbEXd3nOTFEDSKfNACTq4xvs_INIJmaNzcf6SEEaSE20Bm3")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aep4GB4E9ID9mJ4FlkJEYTt_emN5q_CBkyep9a3vpvRBB0UCi_GkaoAx_scAN-vLUH29j1C4EqS8wDE7p98Cx9NV")
        ("y"
         .
         "AeBFX-bOHqmYxzl1aUjPonu4sangygpQJjGDb5bg9d9mnLPKP298D5JAs9h-F0Fhs7BNg5q2_kbyRb1iZQXvS0FU"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(628
      "using secp256k1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "oSY-dbh64JNwYP8UcvMw7lXN-PQynWKEqev7zIVsEWg")
        ("y"
         .
         "QiXnLL6_9B5U-28A4Rr-U6F5N77b8t94f475WE93WDg"))
      #(("crv" . "P-521")
        ("d"
         .
         "ATlqmaM3gh2MktdfVieTxwr6QHSuXm2tK9LMauqPNvbEXd3nOTFEDSKfNACTq4xvs_INIJmaNzcf6SEEaSE20Bm3")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Aep4GB4E9ID9mJ4FlkJEYTt_emN5q_CBkyep9a3vpvRBB0UCi_GkaoAx_scAN-vLUH29j1C4EqS8wDE7p98Cx9NV")
        ("y"
         .
         "AeBFX-bOHqmYxzl1aUjPonu4sangygpQJjGDb5bg9d9mnLPKP298D5JAs9h-F0Fhs7BNg5q2_kbyRb1iZQXvS0FU"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(629
      "Public key uses wrong curve: secp256r1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "UzZtt5syB4GTbfYbtV1EmZSdgT7lq6pd2nDaT5f2gig")
        ("y"
         .
         "zMadfNC3Jmz8KNDcr98-g3OMxhGssI-LiWxOz4LdZa4"))
      #(("crv" . "P-521")
        ("d"
         .
         "ACsNd81cR4kKUmEvybONgEJmuXhKvKK5Temb3GdHWuzCq8Mecqba2gzx1Nd2sALI0tvWAauKDK5BVzcIRrIOjdZX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AXJJDif2ZHz9C_d--fT0Fgqoz5tW4Owv6zKtEsrBSJIA-t9pT_CFC-J3QEPEM9OF_8Oflv73zc9NAJ3PElaggRdM")
        ("y"
         .
         "AS42eHj1z24ItC1YyBM3ie8P-dZ7DPpD5W4oNIJO-ZFTD8QiEzTSKEgh05Lqwh5Mpa8Upu1syEzN1QnO-kTByYdt"))
      #vu8()
      #f
      ())
    #(630
      "Public key uses wrong curve: secp384r1"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "qkXBPOPP6oU4QicSkD7cDOVt907eB3boQ1VaeG-XON4ZQ9_9cprd_UdyFpdR13Za")
        ("y"
         .
         "RbW7VApH0Zj0yMfCHmdWDB4S9wtkUgEJu4hYo_jWu0ASADQx2wd4YzMT_blGTEfs"))
      #(("crv" . "P-521")
        ("d"
         .
         "ACsNd81cR4kKUmEvybONgEJmuXhKvKK5Temb3GdHWuzCq8Mecqba2gzx1Nd2sALI0tvWAauKDK5BVzcIRrIOjdZX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AXJJDif2ZHz9C_d--fT0Fgqoz5tW4Owv6zKtEsrBSJIA-t9pT_CFC-J3QEPEM9OF_8Oflv73zc9NAJ3PElaggRdM")
        ("y"
         .
         "AS42eHj1z24ItC1YyBM3ie8P-dZ7DPpD5W4oNIJO-ZFTD8QiEzTSKEgh05Lqwh5Mpa8Upu1syEzN1QnO-kTByYdt"))
      #vu8()
      #f
      ())
    #(631
      "Public key uses wrong curve: secp256k1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "deAaFVU4C-GI1pqsNApGdeSm9z1jl2oQdSSYJ9jswqM")
        ("y"
         .
         "HmXtHrWRlU4zo49o74qmyTAinYdV5TJXYCs-qofebwI"))
      #(("crv" . "P-521")
        ("d"
         .
         "ACsNd81cR4kKUmEvybONgEJmuXhKvKK5Temb3GdHWuzCq8Mecqba2gzx1Nd2sALI0tvWAauKDK5BVzcIRrIOjdZX")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AXJJDif2ZHz9C_d--fT0Fgqoz5tW4Owv6zKtEsrBSJIA-t9pT_CFC-J3QEPEM9OF_8Oflv73zc9NAJ3PElaggRdM")
        ("y"
         .
         "AS42eHj1z24ItC1YyBM3ie8P-dZ7DPpD5W4oNIJO-ZFTD8QiEzTSKEgh05Lqwh5Mpa8Upu1syEzN1QnO-kTByYdt"))
      #vu8()
      #f
      ())))
(test-ecdh-jwk
  "ecdh_webcrypto_test"
  :algorithm
  "ECDH"
  :curve
  "P-256K"
  :encoding
  "webcrypto"
  :tests
  '(#(632
      "normal case"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "2Alq-KEeC4ADfh7mgka13LsK6xzxJE_XZ9uA8_on2is")
        ("y"
         .
         "OWgS6haG50culpLq8-lY5Q6VANO0x3JD2x8qzWe6nMQ"))
      #(("crv" . "P-256K")
        ("d"
         .
         "9Lf_fMzJiBOmn6498iK_4_Tij3ZL-RtKENgJbORGslQ")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "JDchdVTyxKQl0yCsuVGavln7SRJ5YwyNqo0ZvKptbTI")
        ("y"
         .
         "_B2MI_66Cp6jkx-juqZoTYcKUUiFufhUuh0f2o-zPMM"))
      #vu8(84 77 250 226 42 246 175 147 144 66 177 216 91 113 161 228 158 154 86 20 18 60 77 106 208 200 175 101 186 248 125 101)
      #t
      ())
    #(633
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ll_0LWVOBY7nMXzO18rwk_uxgNjTp0sNzZ2M1Ho51cs")
        ("y"
         .
         "nCqk2qwBpL43wgRn7elkZi8SmD4LUnKkel8nhWhdgIc"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      #t
      ())
    #(634
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BsS4e6dsbcsQH1SgUKCGqiywci8DE331qSJHLxvcEbk")
        ("y"
         .
         "guPHNcS2xIHQkmlVnwgK0IYy83CgVK8Swf0eztLqkhE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2)
      #t
      ())
    #(635
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "u6MO73lnovLwii_62sDkH9TbEqk87wsEW1cG8oU4IeY")
        ("y"
         .
         "1Qsr-Mv1MOYZhp4HwCHvFvaTz8CksNTtWo9GRpK_PW4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3)
      #t
      ())
    #(636
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "FmrtO8KBcFRE-TORP2cJV6EY-Noscb0wGpCSl0Piylg")
        ("y"
         .
         "NRSnly4z1v6h43fvQYSTf2ezfkHvMJnCKKiPW_tn5bk"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(637
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "NuHnb_2-hXdSCwcW64jBjqcqSeWk5WgKfSkAk_hBy24")
        ("y"
         .
         "cxByi1nHVyxLNftsKcNuur_FNVPAbs90f8--_PYRThw"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255)
      #t
      ())
    #(638
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "co4V1XghK8Qih8ARjILISxJvl9VJIjwQrQf06Yr5Ejg")
        ("y"
         .
         "XSOxpucWklhVokexbv_pJ3MxUkGslRzf79-sDtFkZ_Y"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255 0 0 0 0 255 255 255 255)
      #t
      ())
    #(639
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ygP_jpniaVds91ZFRciSaOtBX_RXeHMlKfpZl8wrIwk")
        ("y"
         .
         "UNa4S3KbwH-bLZJ1QoHNwNKJ0kUzha73fkvcab8VXF8"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(0 0 3 255 255 255 0 0 0 63 255 255 240 0 0 3 255 255 255 0 0 0 63 255 255 240 0 0 4 0 0 0)
      #t
      ())
    #(640
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Ub5mE345u_NakcbbW6aRn_Rx2IXKlEYuqqZbHqw2a6o")
        ("y"
         .
         "WRDecLbgnpeqAGIe8Y8oAXGbGZs-d2n9qyvZCbLzQNc"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 0 255 255 0 1 0 2)
      #t
      ())
    #(641
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "I1VlZIUMUPulHx5k75g3jvXCL-r6KUmconYAxHPKzog")
        ("y"
         .
         "nVZ56Rfap_THiZUX03gmKE8DHeAaYLyBNpZBTQRTGiE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(33 12 121 5 115 99 35 89 177 237 180 48 44 17 125 138 19 38 84 105 44 63 238 183 222 58 134 172 63 59 83 247)
      #t
      ())
    #(642
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "3b-AfiLFahnPbEcoKRUDUHgQNKXt3sNlaU1L1chl6tE")
        ("y"
         .
         "TmdBJwKMkdM5TKw3KTqGYFXRDw9Ao3Bq0Wtk_J1ZmL0"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(66 24 242 10 230 198 70 179 99 219 104 96 88 34 251 20 38 76 168 210 88 127 221 111 188 117 13 88 126 118 167 238)
      #t
      ())
    #(643
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WV5G7nwtcYP_Lqdg_9hHL7g07InAi270j_krRKE6bho")
        ("y"
         .
         "5WPiOVPJfCZEEyPSUAyE6M7gTBXU1dLMRYcD0fLQLTE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(127 255 0 1 255 252 0 7 255 240 0 31 255 192 0 127 255 0 1 255 252 0 7 255 240 0 31 255 192 0 127 255)
      #t
      ())
    #(644
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5Cbi9RCDMxF1h5dfGNjMB41B5Wt9a4L1hddbDXNHn_0")
        ("y"
         .
         "dYAP1BI2pWA0vtmrxV2CzwWaFNY8B80HUJMXFHMaHKE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      #t
      ())
    #(645
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "4ccHbK8mAQsXZ_GpxBVrW0I2No1dkN7ONEG3NOhoTuY")
        ("y"
         .
         "s1NMPFTmFOWU3ObKQ4uHxCTI6A-PriJrvfUOSQbBP2s"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(128 0 0 63 255 255 240 0 0 7 255 255 254 0 0 0 255 255 255 192 0 0 31 255 255 248 0 0 4 0 0 1)
      #t
      ())
    #(646
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ZjzqEGPJkWt16F_IFdiiNw7AoCrO7z2wIuOV24sDvz8")
        ("y"
         .
         "GIeH9AR9wQaAdSZQLHrogORxySm5LiOESJyAcLW8wQk"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(255 0 0 0 1 255 255 255 252 0 0 0 7 255 255 255 240 0 0 0 31 255 255 255 192 0 0 0 128 0 0 0)
      #t
      ())
    #(647
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "JBdcB44wXTE55dq3J6arhYeybapHClKaI8EFhctWwDg")
        ("y"
         .
         "vx8rk3rgdP-UsV9cteYOtdMq-6IHdTnbeUKUvKq3GoE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(255 255 0 0 0 3 255 255 255 240 0 0 0 63 255 255 255 0 0 0 3 255 255 255 240 0 0 0 63 255 255 255)
      #t
      ())
    #(648
      "edge case for shared secret"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AI1xxxLdlYgc0UANvnaDrL2OJp0lJhsI8fSRtF47ViE")
        ("y"
         .
         "d4GCokGYsPI1AtBuJMRRIuH0IK9I3B4XseqSM4ajMGI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(255 255 255 255 0 0 0 0 0 0 0 255 255 255 255 255 255 255 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
      #t
      ())
    #(649
      "y-coordinate of the public key has many trailing 1's"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Vrrx1yYGx69aX6EIYgsIOeLH3UC4Mu-EfltkyG7-GqU")
        ("y"
         .
         "Y-WGpmemW7tWklAN8f-EA3NoOLMOqXkdnTkOPcZoniw"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(128 0 0 0 0 0 0 0 0 0 0 0 0 159 162 241 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(650
      "y-coordinate of the public key is small"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Xkws8TIOyE74kghntAmpqR0t0AghaiguNr2E6IRyb6A")
        ("y"
         .
         "Wl5K8Rz2POqqQqbcnkzLOUhSz4QoTo0mJ1cvvyLAuog"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(128 0 0 0 0 0 0 0 0 0 0 0 0 163 3 126 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(651
      "y-coordinate of the public key is small"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AqMML6vIfmcwYl3sLw0DiUOHt_dDzmnEc1Hr5e6YpIM")
        ("y"
         .
         "B-t404dw_qGkT02nLCb4WxfzUBpPk5T-KYVsy_Ff0oQ"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(128 0 0 0 0 0 0 0 0 0 0 0 1 36 220 176 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(652
      "y-coordinate of the public key is large"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Xkws8TIOyE74kghntAmpqR0t0AghaiguNr2E6IRyb6A")
        ("y"
         .
         "paG1DuMJwxVVvVkjYbM0xretMHvXsXLZ2KjQP90_Qac"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(128 0 0 0 0 0 0 0 0 0 0 0 0 163 3 126 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(653
      "y-coordinate of the public key is large"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AqMML6vIfmcwYl3sLw0DiUOHt_dDzmnEc1Hr5e6YpIM")
        ("y"
         .
         "-BSHLHiPAV5bsLJY09kHpOgMr-WwbGsB1nqTMw6gKas"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(128 0 0 0 0 0 0 0 0 0 0 0 1 36 220 176 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(654
      "y-coordinate of the public key has many trailing 0's"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "VFDKzgQ4atxUoUNQeT6DvcXyZdbCkofs0H95GtJ4TEw")
        ("y"
         .
         "69PCRFEyIzTY1RAz6dNLa7WSsZldB4Z4Y9EES9WddQE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "orZEKjf4o3ZK7_QBGkxCKziaHlCWacQ_J5yLfjLYDDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5PmP6NWWKNzK0al9AnyrtIM3t54tGs-pnwggXqijyBI")
        ("y"
         .
         "sjeCPt9b0dObBwhL_Pco7O6J6tO_09NkO50dp3hwc00"))
      #vu8(128 0 0 0 0 0 0 0 0 0 0 0 1 18 107 84 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)
      #t
      ())
    #(655
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE")
        ("y"
         .
         "QhjyCubGRrNj22hgWCL7FCZMqNJYf91vvHUNWH52p-4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(18 194 173 54 165 159 218 90 196 247 233 127 246 17 114 141 7 72 172 53 159 202 155 18 246 212 244 53 25 81 100 135)
      #t
      ())
    #(656
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAI")
        ("y"
         .
         "ZvvnJ7K6CeCfWpjXCl786EJMX6Qlu9ocUR-GBle4U14"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(69 170 150 102 117 120 21 233 151 65 64 209 181 113 145 201 44 88 143 110 86 129 19 30 13 249 179 210 65 131 26 212)
      #t
      ())
    #(657
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAM")
        ("y"
         .
         "LyMzlciwejg0oOWb2kOUS13zeIUuVg68DyKHfp9Ju0s"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(185 9 100 192 94 70 76 35 172 183 71 164 200 53 17 233 48 7 247 73 155 6 92 142 142 204 236 149 93 135 49 244)
      #t
      ())
    #(658
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAP____________________8")
        ("y"
         .
         "PbdyrZLbhpnOrBo8MOEmuGbE_v4pLPDBeQ5VzuhBTxg"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(129 99 201 220 232 53 111 29 247 43 105 143 47 4 161 77 176 38 58 132 2 144 94 238 135 148 27 0 216 214 119 245)
      #t
      ())
    #(659
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAD__________wAAAAAAAAAA__________8")
        ("y"
         .
         "Mc8TZxtXTjE8NSF1ZvGL0sX3WMFA0k6U5qT9p_THsSs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(26 50 116 157 207 4 122 126 6 25 76 203 52 215 201 83 138 22 221 171 238 237 231 75 234 95 126 240 73 121 247 247)
      #t
      ())
    #(660
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAP____8AAAAA_____wAAAAD_____AAAAAP____8")
        ("y"
         .
         "c7CIZJau1w2zceLknbZAq7pUfl4MJ2O3OgpC-ENIprE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(171 67 145 122 100 193 176 16 21 150 67 193 142 46 176 109 37 238 218 229 183 141 2 250 155 61 235 172 191 49 183 119)
      #t
      ())
    #(661
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAD____AAAAP___8AAAA____wAAAD____AAAAQAAAA")
        ("y"
         .
         "D02BV1yOMoKFYzzP2GI_BN1O1h4YezptfqxVOu3n-FA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(22 72 50 28 112 102 81 173 240 102 67 252 74 224 96 65 220 230 74 130 99 42 212 65 40 6 18 22 204 152 39 255)
      #t
      ())
    #(662
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAD__wAA__8AAP__AAD__wAA__8AAP__AAD__wABAAI")
        ("y"
         .
         "QiF7cFmz3evGjpVEP2wQk2nh-TI90khSrHYSmWtuVgE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(251 134 107 46 75 31 158 214 179 120 71 252 128 161 154 82 225 233 27 117 215 19 176 212 246 185 149 210 211 199 92 254)
      #t
      ())
    #(663
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "IQx5BXNjI1mx7bQwLBF9ihMmVGksP-633jqGrD87U_c")
        ("y"
         .
         "X0UNu_cYpPZYLXr4OVMXCzA3-4GkUKXKWsvsdK1srIk"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(25 8 174 147 111 83 185 168 162 208 151 7 174 65 64 132 9 11 23 83 101 64 20 37 71 155 16 184 195 232 209 186)
      #t
      ())
    #(664
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "QhjyCubGRrNj22hgWCL7FCZMqNJYf91vvHUNWH52p-4")
        ("y"
         .
         "NyaaZLvPOj8idjHHqM5TLHckWhwNtDQ_FqodM5_SWRo"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(94 19 179 220 4 227 63 24 209 40 108 96 108 176 25 23 133 246 148 232 46 23 121 97 69 201 231 180 155 194 175 88)
      #t
      ())
    #(665
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "f_8AAf_8AAf_8AAf_8AAf_8AAf_8AAf_8AAf_8AAf_8")
        ("y"
         .
         "S2YAPHSC0PL9exyysLcHjNGZ8iCPw36y7yhsyy8SJOc"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(49 53 166 40 59 151 231 83 122 139 194 8 163 85 194 168 84 184 238 110 66 39 32 103 48 230 215 37 218 4 77 238)
      #t
      ())
    #(666
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "adPNDHDxSE1LO7vWgGee9HeiKgffCFY08RfEHAi_EjA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(98 149 144 137 167 237 71 124 34 203 79 28 119 135 50 115 24 252 204 162 94 90 163 228 70 136 162 130 147 26 176 73)
      #t
      ())
    #(667
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gAAAP___8AAAB____gAAAP___8AAAB____gAAAQAAAE")
        ("y"
         .
         "MPabbpWjMDIUpzrZgqHz7hadfs-VjeewvKip_6O46LM"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(4 253 165 192 10 57 111 173 107 128 154 136 67 222 87 62 134 176 64 61 100 73 149 200 51 19 218 81 251 31 88 128)
      #t
      ())
    #(668
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_wAAAAH____8AAAAB_____AAAAAf____wAAAAIAAAAA")
        ("y"
         .
         "VpUerYYaqOx6MU_NVPkFvZLJEHhjdet-5fOlX4qoeIQ"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(187 217 147 123 181 29 39 249 78 202 234 41 113 125 247 137 175 234 196 65 78 62 242 123 178 230 250 114 89 24 46 89)
      #t
      ())
    #(669
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__8AAAAD____8AAAAD____8AAAAD____8AAAAD____8")
        ("y"
         .
         "Y6iLLgyJh8YxDPgdDJNfACE_mKPa0vQ8gSj6MTqQ1Vs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(187 217 211 5 185 159 243 219 86 247 127 234 158 137 243 34 96 238 115 38 4 0 103 206 5 221 21 224 220 193 62 216)
      #t
      ())
    #(670
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____wAAAAAAAAD_________AAAAAAAAAQAAAAAAAAA")
        ("y"
         .
         "ZqRFbKbUBU0Tsgn20mLmBXrXElZvRuniOOiU3uvj06o"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(79 251 44 121 98 227 45 83 101 249 143 102 190 98 134 114 77 64 213 240 51 59 164 252 148 60 15 15 6 205 187 31)
      #t
      ())
    #(671
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gAAAAAAAAAAAAAAAAJ-i8f____________________8")
        ("y"
         .
         "B-01PJ8QOe3MnMUzbANNwTGkCHaSwuVrwd0ZBOP___8"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(124 7 177 153 182 166 46 122 198 70 199 225 222 233 74 202 85 222 26 151 37 29 223 146 252 212 254 1 69 180 15 18)
      #t
      ())
    #(672
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gAAAAAAAAAAAAAAAAKMDfv____________________8")
        ("y"
         .
         "AAADGmvzRLhnMKxcVKd1Gu_boTV1m51TXKZBEfKYo40"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(82 6 195 222 70 148 155 157 161 96 41 94 224 170 20 47 227 230 98 156 194 94 45 103 30 88 46 48 255 135 80 130)
      #t
      ())
    #(673
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gAAAAAAAAAAAAAAAAKMDfv____________________8")
        ("y"
         .
         "___85ZQMu0eYz1Ojq1iK5RAkXsqKZGKso1m-7Q1nWKI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(82 6 195 222 70 148 155 157 161 96 41 94 224 170 20 47 227 230 98 156 194 94 45 103 30 88 46 48 255 135 80 130)
      #t
      ())
    #(674
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gAAAAAAAAAAAAAAAARJrVP____________________8")
        ("y"
         .
         "QQajaQaNRU6kucOsYXf4f8j9OqJAssy0iCvcy9QAAAA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(229 157 220 118 70 228 174 240 98 60 113 196 134 242 77 93 50 247 37 126 243 218 184 250 82 75 57 78 174 25 235 225)
      #t
      ())
    #(675
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gAAAAAAAAAAAAAAAASTcsP____________________8")
        ("y"
         .
         "AAABO8bwhDHnKe0oY_L0rIowJ5aVyBCcNAo5-ob0Uc0"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(138 140 24 183 142 27 31 207 210 46 225 139 74 58 159 57 26 63 223 21 64 143 183 248 193 219 163 60 39 29 189 47)
      #t
      ())
    #(676
      "edge cases for ephemeral key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gAAAAAAAAAAAAAAAASTcsP____________________8")
        ("y"
         .
         "___-xDkPe84Y1hLXnA0LU3XP2GlqN-9jy_XGBHkLqmI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "K8Fc85geq2HllOv1kSkKBFypMmqNPdSfPeEZDTknC7g")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lKA-vBLDAkbhDSmlii3fWM-bE1OfcxlCZGqN2bstPGM")
        ("y"
         .
         "3zp08U9IM2Q-K1ylRtO7o5JWqLnxgxiUPZXMoHnQq0Q"))
      #vu8(138 140 24 183 142 27 31 207 210 46 225 139 74 58 159 57 26 63 223 21 64 143 183 248 193 219 163 60 39 29 189 47)
      #t
      ())
    #(677
      "point with coordinate x = 1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE")
        ("y"
         .
         "QhjyCubGRrNj22hgWCL7FCZMqNJYf91vvHUNWH52p-4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(243 98 40 157 58 128 61 86 138 10 66 118 112 112 215 147 189 112 137 31 181 224 59 1 65 59 109 63 30 181 47 248)
      #t
      ())
    #(678
      "point with coordinate x = 1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Irlh7RT2NokD_utC1j03vREwKJPo_2TBqOf9BzFDm7Y")
        ("y"
         .
         "mBpxIGO_ujTRd0ErsoTENhlT3s8pu94BhaWL0C875DA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(186 226 41 230 208 1 253 71 116 26 238 232 96 4 138 133 84 50 7 111 226 112 246 50 244 109 19 1 2 43 100 82)
      #t
      ())
    #(679
      "point with coordinate x = 1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "L5JFusb6lZtPAnc-IUEfSLdPmAb-TTLja9-asCgU81M")
        ("y"
         .
         "V0XaM00Guv4tg8I18Mein48EJyLsNOU6qW2XozGnM-8"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(233 11 158 129 189 1 61 52 159 112 253 225 181 27 173 4 197 129 1 28 104 240 194 5 58 201 29 200 24 122 187 154)
      #t
      ())
    #(680
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "HjlqJSW8P7AK-Jiwa7h8HWdPwGYrhn_6wI6w26IUbCE")
        ("y"
         .
         "qLhCnxGANkm-NK5RXBc6Q7p08T670OJhARwWLlc1mbQ"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(222 124 222 107 88 6 163 37 218 132 94 154 25 30 24 55 120 104 99 110 94 241 245 250 8 231 86 192 45 111 212 222)
      #t
      ())
    #(681
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "RSFYMDs3_5vrynIOoQhe-qT4WduVCpn8zZ0tF5Jzq7E")
        ("y"
         .
         "CKkIP4B1AFlDvWjFZuwfLwZ2ZNqSEuwYM3mbuogdjos"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(150 50 143 168 64 56 166 60 129 126 247 205 19 183 151 148 162 219 70 125 211 189 135 105 223 120 42 218 206 60 130 235)
      #t
      ())
    #(682
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WipAFmbg8ZeMbzCuxT_uWLTE9158GgAVajatJ8ClopU")
        ("y"
         .
         "ZYV35lciO4wgyCYkO1riyg9hSMJSnsbWDsJgkWZB2Po"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(182 105 159 233 161 140 45 13 20 233 84 5 19 62 0 11 22 125 194 229 69 29 205 240 154 222 73 186 13 178 19 235)
      #t
      ())
    #(683
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ss0DlQC89GDiT9gDg7YOuBpW9GcHfnaCMVU6D6Da_Mg")
        ("y"
         .
         "HUobj9iLOyPy1QMoXJ1yu6RIwVvAFsYg9wdZmhKVRq4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(122 35 174 229 176 254 209 102 56 240 226 235 5 251 161 253 68 22 125 73 110 190 178 116 219 33 133 147 180 234 32 28)
      #t
      ())
    #(684
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "A89QDYOMn8uX2N26JGbsbkdEmDFdbCpDEQMI8iRZ1Js")
        ("y"
         .
         "B4dartLtq-2EL7FgjKcGvTnWAhpgvCcJR8EgU8nbr6E"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(216 174 244 197 200 182 8 134 215 243 60 221 35 144 194 19 17 230 159 96 109 199 119 220 65 196 58 70 25 149 195 21)
      #t
      ())
    #(685
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "C28stilU-ZRWThQZz51ZguxlEef6fhf5aF4BmUmQbfI")
        ("y"
         .
         "-0KbBVSiWkoMUQJw08xz5s_sm8LmPL0reqDbmOHzMM0"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(76 8 146 186 205 152 62 192 1 63 71 125 148 216 251 133 5 133 239 242 25 123 83 213 102 169 146 107 216 152 217 72)
      #t
      ())
    #(686
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MCWwRvSnDgYzDzsUxPo-weBP4Z7YyQNS3G_1Ynyns7M")
        ("y"
         .
         "omTVrZ8G2Eh0MPZU992PZzX8g270jW1NSZapwgrzIO4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(110 33 25 147 90 92 37 33 24 42 112 29 90 19 33 90 125 251 138 31 0 27 56 135 232 174 81 191 37 155 24 11)
      #t
      ())
    #(687
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "fTuEKMgCmaT_HZbtdaWkRin9AxPAl8R45V8voK5FtpE")
        ("y"
         .
         "u0ljtcwJWr5dz-mDmTF_1a1Z82dMBwY6kSOiqiSBRYU"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(43 139 13 78 238 131 209 196 177 242 166 113 68 254 247 142 127 170 134 230 213 214 168 183 43 53 156 79 55 58 219 113)
      #t
      ())
    #(688
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "-3eEGITTD6W3OtIdClrkDFOp-qXTJWmUNjOM7kuiE2k")
        ("y"
         .
         "e9cy1HyG3NY2kQl7GZnJ8KZgqcPWE2cQOc8XY0Z9gUA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(88 219 246 173 94 121 25 86 232 99 84 39 37 44 245 229 24 189 16 237 42 147 106 31 55 71 186 78 167 152 50 116)
      #t
      ())
    #(689
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "mu9RODpVbPMX_mi-pqhaJUgl7Fs_E1iuogmkPKOMZjU")
        ("y"
         .
         "Gu4aka6ypNytc5ci9ExDff0ycx8IYt11G4C9iNCWm9Y"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(18 59 73 64 88 222 195 47 183 79 222 177 170 154 77 82 191 221 242 217 11 155 70 237 188 244 204 149 234 115 98 81)
      #t
      ())
    #(690
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "J2nfM1Ey8gJeZBBIBGeHN4YO5m4H5nX3IOfU71w4osI")
        ("y"
         .
         "gfgMO21H2wpBLm7dPFv0isysFJe1nhOxXfws0V5q5Ho"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(175 75 121 239 196 80 99 11 140 2 118 151 226 55 111 20 132 43 171 173 129 189 104 89 44 55 39 154 159 196 26 182)
      #t
      ())
    #(691
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "YbjEh1BzWmtcxFtpGQblzCktWWm7HW_5D_RG2UgRznw")
        ("y"
         .
         "KFOXdBnLorksxXSrzgMEc-sAg1BWbX6qJMudqtcHEO0"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(8 180 50 107 66 216 23 226 248 188 9 242 111 73 183 32 175 206 222 16 35 109 10 37 231 233 81 142 172 150 227 189)
      #t
      ())
    #(692
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Fu6ryALjQJt8ez52B7cWYkP8F0YpSUj8gSOzmc-4mWI")
        ("y"
         .
         "_L8L-KUZHOlY3V6jq2M8CQ0SWfvZqXf94MwhLVs7mFg"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(82 80 91 254 169 208 102 240 170 144 8 232 56 35 130 199 212 132 96 215 111 41 98 229 9 72 43 110 181 110 10 197)
      #t
      ())
    #(693
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Y7HZlJG0bO5-GGokO84cw4YpdXGFChDZopYtdppBHGE")
        ("y"
         .
         "Y0XihTLKw5lgovErvQMgW3dGSoCgQWRG5v-FhRoAn2Q"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(30 100 100 247 143 190 222 205 130 26 79 160 77 126 143 19 100 211 36 190 36 209 34 18 153 70 131 252 43 107 177 162)
      #t
      ())
    #(694
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dceK6clGE90FHu19199moIHNCsJ89l5O8OqCYnbF788")
        ("y"
         .
         "qS7RxP-7hDAfW7HGvJ4Uxuba0eBKKHgmUoR4-a4WCcI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(180 252 183 45 31 129 219 33 29 217 64 57 161 54 140 44 78 255 209 239 232 50 241 161 219 45 174 82 83 194 115 3)
      #t
      ())
    #(695
      "point with coordinate x = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Byg45Ply1KZdJY28MqUw_eLoc1N7WkqnB8-BzswPf_E")
        ("y"
         .
         "Lktgi54yHJ23LPTZuks8LBN1YEDXeva9JRvCTPGGdvE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "k489vjcTXNjIxIpnayiyM0tyo_CYFMjvtqRRvgDJPSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WHy-GsjfUTNUTcTZDst-oapmhJ2O0YKebuVuekxZ2eQ")
        ("y"
         .
         "ypl0ClW1RBvbkY1LsCcRhbBAaGSDvbdyZnUM5xOwRbM"))
      #vu8(244 244 146 107 111 100 228 122 190 173 189 197 168 166 119 6 164 46 0 119 78 28 197 175 218 125 87 206 214 66 59 57)
      #t
      ())
    #(696
      "point with coordinate x = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "QwH1SzWS0eoqQJiclCYdKx0f4pftbtZBJe4kHeBdAEs")
        ("y"
         .
         "x5AU8Vbpt7-za4rS1m1V86dTgpqd24YFW7kWbdOv9Fc"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(253 193 90 38 171 186 222 52 22 225 32 26 109 115 113 40 162 248 151 240 216 129 8 100 84 83 161 179 221 208 86 136)
      #t
      ())
    #(697
      "point with coordinate x = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "NrD2a_X5_Ust-c2uKvhzoHXFVJfX_sRzenyWQ8LHb-U")
        ("y"
         .
         "2p9yh7PNTl8FuaGk9k6KjZbDFuRSWU0CpFkqIQfs6Qs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(227 99 72 227 164 100 188 81 131 132 128 108 84 142 21 110 221 153 76 182 148 100 115 194 101 162 73 20 213 85 159 28)
      #t
      ())
    #(698
      "point with coordinate x = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "gqu1ivti0mGHi97hJmTfFJm4JPHWD7AoEWQssC9K_10")
        ("y"
         .
         "MHGYNdlvMtwDxJ2BX_ohKFczE39QfOMWzsZcpWLOKtA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(125 101 104 75 220 228 172 149 219 0 47 186 53 13 200 157 13 15 201 225 34 96 208 24 104 84 63 42 108 140 91 141)
      #t
      ())
    #(699
      "point with coordinate x = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "fee3z1xf9CQNrzGlCsbPaxaarQfSxZNsc7g-45h-IqE")
        ("y"
         .
         "lAwb145L5mklhcmdyStHZx4sy88SqamFTGYH-YITwQg"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(110 198 186 35 116 171 10 154 230 99 243 247 54 113 21 138 170 186 195 172 104 157 108 39 2 235 223 65 134 89 122 133)
      #t
      ())
    #(700
      "point with coordinate x = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BvqTUnKUyFM6pAHOTmyK6wWmkhvEh5io4goPhKUIWvQ")
        ("y"
         .
         "7Ego-DlNIt5DBDEXuFlfsRMkX3KFyzVDk4noVHoQUDk"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(109 110 135 120 125 10 148 126 207 191 121 98 20 47 222 143 249 181 144 228 114 192 196 107 188 93 57 2 14 79 120 167)
      #t
      ())
    #(701
      "point with coordinate x = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ik9iUhC0SNyEatI5mzHNG8PxeIx77WnMHLeqyKso1Tk")
        ("y"
         .
         "MAfG8R8-JI3mUcZiLeMI7lV2voTvHtjtkf0kTxT8IFM"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(86 234 67 130 248 225 171 252 178 17 152 159 80 6 118 68 154 188 235 254 44 210 32 77 216 146 61 235 83 10 108 123)
      #t
      ())
    #(702
      "point with coordinate x = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "iF5FLLsOSyqXaLdZbBUxmKki2ru40Modw_r08JfwkRM")
        ("y"
         .
         "vpqqYwkY1QVgU-z3OI9Ei5EtnM--2A18ojwOeZGjSQE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(44 54 44 39 179 16 126 168 160 66 192 92 197 12 74 141 218 174 140 220 51 208 88 73 41 81 160 63 141 143 129 148)
      #t
      ())
    #(703
      "point with coordinate x = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "4ibfH898E3pBySD_dNYgT6ogk-7_xKnuCiP7LplAQcM")
        ("y"
         .
         "RXEHRCzEs69jHE37X1PixWCL7QT_ZlO3cffNRnD4EDQ"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(1 136 218 40 156 232 151 74 79 68 82 9 96 250 232 179 83 117 10 202 120 146 114 233 249 13 18 21 186 205 216 112)
      #t
      ())
    #(704
      "point with coordinate x = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "9T6tlXXuu6Ow6w0DOst-mTiOhZC0rS216k9r2b3haZU")
        ("y"
         .
         "tfOrFflzyp46qd_ikU7rvS4RAQtFVROQeQiAA5b7nRo"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(247 139 215 255 137 156 129 184 102 190 23 192 169 75 236 89 40 56 215 141 31 12 12 245 50 130 155 108 70 76 40 172)
      #t
      ())
    #(705
      "point with coordinate x = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "iCdz7H4QYFyPni47hwCUO-JrzEydH-3yvc-zaZTyPH8")
        ("y"
         .
         "jl0Fsv3SlUthiHNuvj9WRmAqWNl4txa1ME6lZ3dpHbM"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(153 246 21 31 186 40 6 126 172 115 53 73 32 252 193 250 23 254 166 50 37 165 131 50 60 182 195 212 5 78 202 202)
      #t
      ())
    #(706
      "point with coordinate x = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "pgtkWCVrONRkRFG0kL01f-ree7a4RTwfyJeU1aRfdo0")
        ("y"
         .
         "ge7pBUilnl0s7NctSwteZXTWWp2DfHxZDR0SXuN8TVE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(104 202 57 222 12 236 34 151 82 159 86 135 107 195 222 123 227 112 243 0 232 124 43 9 205 187 81 32 56 45 105 119)
      #t
      ())
    #(707
      "point with coordinate y = 1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y7DeqxJXVPH9sgOLBDTtnLP7U6tzU5ESmZSlNdkl9nM")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(175 48 108 153 61 238 13 207 196 65 235 229 51 96 181 105 226 31 24 96 82 219 129 151 244 161 36 250 119 185 129 72)
      #t
      ())
    #(708
      "point with coordinate y = 1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "JIAN6sP-THZbbeyA6imddxraTzDk4VazrLcg26NzlHE")
        ("y"
         .
         "X-TGS7BkjibQXLnMmKyG1Ol7i_Evkrmy_cOuzY6mZIs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(170 127 201 254 96 68 94 172 36 81 236 36 193 164 73 9 132 47 161 64 37 242 161 211 221 127 49 1 159 150 43 229)
      #t
      ())
    #(709
      "point with coordinate y = 1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "jzNlL1vaLDKVPr8tLsqV4FsXyKt9mWAb7kRd-ETUajY")
        ("y"
         .
         "nPWsAHcRvb5cAzPcDAY2pkgj7kgBlGSUDR8n4FxCCN4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(8 42 67 168 65 119 130 167 149 200 212 199 15 67 237 202 187 194 69 168 130 10 192 27 233 12 26 207 3 67 186 145)
      #t
      ())
    #(710
      "point with coordinate y = 1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "FG07Za3Z9UzMooUzyI4svGP3RD4WWHg6tB-O-XwqELU")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(112 129 11 71 128 166 60 134 4 39 211 160 38 159 108 157 60 46 163 52 148 197 14 88 162 11 148 128 3 75 199 160)
      #t
      ())
    #(711
      "point with coordinate y = 1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sDREGKRQTAfnkh7Z8AcUtdOQ5cteeTuxRl9zF09sJv4")
        ("y"
         .
         "X-TGS7BkjibQXLnMmKyG1Ol7i_Evkrmy_cOuzY6mZIs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(167 211 78 226 95 187 53 79 134 56 211 24 80 218 180 30 75 8 104 134 247 237 63 45 110 3 91 206 184 202 184 160)
      #t
      ())
    #(712
      "point with coordinate y = 1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ipjBvGvnXFeWvksp3YhcNIXnXje0zKybNyUeZxdf8NY")
        ("y"
         .
         "nPWsAHcRvb5cAzPcDAY2pkgj7kgBlGSUDR8n4FxCCN4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(63 9 203 193 46 209 112 31 89 221 90 168 61 174 245 230 103 106 223 127 210 53 197 63 105 174 181 213 182 119 153 224)
      #t
      ())
    #(713
      "point with coordinate y = 1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "H-Hl7z_OtcE1q3dBMzzlpugNaBZ2U_ayskvLz6qv9Qc")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(224 78 136 31 65 107 181 170 55 150 64 122 165 255 221 248 225 178 68 107 24 95 112 15 105 83 70 131 132 250 175 118)
      #t
      ())
    #(714
      "point with coordinate y = 1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "K0ut_JexZ4G8__SlJc9N0xGUywO8pW2bDOlsDA0gQMA")
        ("y"
         .
         "X-TGS7BkjibQXLnMmKyG1Ol7i_Evkrmy_cOuzY6mZIs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(173 172 231 31 64 0 108 4 85 117 64 194 237 129 2 216 48 199 246 56 226 32 30 254 180 125 115 45 167 159 19 217)
      #t
      ())
    #(715
      "point with coordinate y = 1"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5jPZFDg-d3XUAvWo860N6x8A2RzNmfNI2paDnqPLnVI")
        ("y"
         .
         "nPWsAHcRvb5cAzPcDAY2pkgj7kgBlGSUDR8n4FxCCN4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(184 203 240 150 143 183 13 57 16 89 208 144 179 13 28 78 220 210 218 215 171 191 122 164 173 69 47 90 70 68 167 190)
      #t
      ())
    #(716
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "0cG1Ccndt2IhoGaiKjwzP-5eHS0aS6veSh0z7CR6fqM")
        ("y"
         .
         "AWL5VFNOrbG06pXFfUChAhTlt0buaqQZTtKyASty-X0"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(7 37 114 69 218 75 194 102 150 226 69 83 28 122 151 194 181 41 241 202 45 140 5 22 38 82 14 107 131 215 250 242)
      #t
      ())
    #(717
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "dV2IRee0_ScDU_aZnpckIiQBVSe_P5TMLGk9G2uhIpg")
        ("y"
         .
         "YE-BdONgW48YvtN0K2hxqM_84AbbMbjX2Db1DPzafRY"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(214 170 64 27 156 225 126 207 125 215 176 134 29 254 179 107 177 116 157 18 83 57 145 230 108 13 148 34 129 174 19 171)
      #t
      ())
    #(718
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "xvn8hkS6XJ6pvrEs4suRHFSH6LG-kdWhaDGPSuRNZoA")
        ("y"
         .
         "e8M3ocguPF96KSeYe4-uE2JyN9Ig-vtAExI7-9lfC6U"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(244 59 254 78 204 194 78 191 110 54 197 188 172 164 123 119 12 23 188 181 158 167 136 177 92 116 174 108 157 208 85 161)
      #t
      ())
    #(719
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "0xefzleB0MSc6EgKgR9vCOPxI9n2AQ-_YZtdhoqOqDM")
        ("y"
         .
         "3fmmZr8AFbIOSRL3D2Ve8huCCHWWqh4vHihlNQ0VkYU"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(0 155 195 171 179 207 10 202 33 79 14 141 181 8 141 82 11 61 74 173 177 212 76 74 43 231 240 49 70 28 148 32)
      #t
      ())
    #(720
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ngmAlUY8kaxxB6kgzLJ21F4fckDvK5O5V-4JOT0y4AE")
        ("y"
         .
         "UDr0ouOyYnlWT-2OdyoEPnVjDk44WZdu3oj_zxb1ynE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(139 203 7 163 208 250 130 175 96 200 138 141 103 129 14 188 160 234 39 84 131 132 233 109 52 131 49 2 18 33 147 18)
      #t
      ())
    #(721
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "vzA0qZNRgto2JXAxUBFUSsLOipwid3wvx2esnFwNrus")
        ("y"
         .
         "zzM1YvPgGIkjdDU2dN6EkPydMEJlmOtgB3kVS68q7Bc"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(160 157 220 124 254 2 58 205 149 113 239 7 84 1 2 137 200 4 103 140 4 63 144 15 38 145 221 128 27 148 46 212)
      #t
      ())
    #(722
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "cJxxecK7J845hbpC_rhw8Gnazq2SlMgFV76IL7V3kEg")
        ("y"
         .
         "Hm_iwacVFj76-G6oseVepXQtawQubL-KzGnJn4JxqQI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(218 152 5 77 81 172 150 21 233 212 245 206 218 31 27 173 64 48 42 193 22 3 67 30 254 193 58 181 14 50 252 242)
      #t
      ())
    #(723
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "JkwAotklFKbb5lXePHGldAzsT8slGqSMpnRdvqb1988")
        ("y"
         .
         "wdXun8POSf1FCdM8Tc_MGiCmYFKfqevW5q_D1chMcrs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(214 7 149 216 243 16 177 85 114 101 52 184 190 61 11 138 123 194 206 212 104 198 230 76 139 154 224 135 179 62 224 11)
      #t
      ())
    #(724
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "oSEkYGvLuzPOzsf8jXiziXGSyoUVYMU55H3SdsY708I")
        ("y"
         .
         "8goMphi6ATGi43PzH3Oz9V6RiNRv3bxjh-Mq77nzuhI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(103 95 239 143 86 128 191 118 34 14 145 54 38 19 148 64 153 4 107 11 160 126 88 36 233 63 62 60 194 204 39 88)
      #t
      ())
    #(725
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "JEt6_n8xKJ-daq639w0pp7SaIox7sgJ2SrqU2qqjMyI")
        ("y"
         .
         "cMYJdXSPDHSaiw-PweIi3cvTOE9taPC2tv9nm0Nc3LE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(118 180 57 248 234 123 66 241 28 213 158 109 145 178 210 167 37 119 193 133 56 107 106 246 99 155 232 227 134 74 127 39)
      #t
      ())
    #(726
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KsKdsuvE-pRztCvTNaYCJlecwYayxnajsBvGDliWFhY")
        ("y"
         .
         "WqnA0bJA5t1CEeMjVCVjSyeK2I_t4DN9Ws8xNlh9hBM"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(86 230 63 167 136 18 29 94 250 12 227 202 244 96 90 241 141 72 198 49 73 108 223 168 98 196 62 207 94 95 193 39)
      #t
      ())
    #(727
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "5iruUgWoBj465AHVPpNDAB5V619OTWtw4rhBWc8xV-Y")
        ("y"
         .
         "S6LkIMq8Q7bo6GWQ_COD0Xgn3ZmmDCEfGQp0JpEAwUE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(207 243 181 225 158 214 126 81 17 221 118 227 16 161 241 29 127 153 169 63 190 156 197 198 243 56 64 134 202 205 17 66)
      #t
      ())
    #(728
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mdzm3nQfECZ_Lo89VypPSb5f5S_3v_PDtGRvOAdsBnU")
        ("y"
         .
         "JwKlFamlDbHYb9Qq6gg02utivgPQzZAz-EucS1ahnxI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(226 148 131 136 74 116 251 132 244 96 22 84 136 90 15 87 70 145 57 79 6 78 166 147 122 132 97 117 239 8 31 197)
      #t
      ())
    #(729
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ZRjNZrHYQeaJ1dxmdMfMfZZFdNFJD_95Br03NJR5FZk")
        ("y"
         .
         "EEJ3FwaS-mvyJwWA1W0byBtU9HfYq2w_WEJlCscXbXE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(156 106 75 203 47 192 134 172 168 114 109 133 15 167 153 32 33 74 244 193 81 172 234 15 207 18 167 105 173 31 53 116)
      #t
      ())
    #(730
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lSqIzjGtTLCGl45sViHD2AI7LBFBjW_Q3O-N5yEj78E")
        ("y"
         .
         "XTZ2iP3l4ILwl4VaDArcMF3Wz0b1DKdYWbskO3AklgU"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(52 183 171 195 243 227 110 55 226 213 114 138 135 10 41 58 22 64 49 70 202 103 255 145 203 171 238 226 187 46 3 139)
      #t
      ())
    #(731
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "KkPzNXO2GXGQmc9U9szLKNFt85kiOfrfecesucZPevA")
        ("y"
         .
         "9NHSKvcYfI3huZKkBGxBm4Ac3lfWONMPLhrEk1MReiA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(155 209 40 79 27 203 25 52 212 131 131 76 174 65 167 125 178 140 217 85 56 105 56 71 85 182 152 63 79 56 72 160)
      #t
      ())
    #(732
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "GxsMdUCHhehHJ7DlXkuiDQ8lmcTtCEgtwfO131RWkTg")
        ("y"
         .
         "AWL5VFNOrbG06pXFfUChAhTlt0buaqQZTtKyASty-X0"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(22 126 61 182 169 18 172 97 23 100 69 37 145 31 200 135 46 211 59 142 11 189 80 7 61 211 193 122 116 78 97 224)
      #t
      ())
    #(733
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "TdEoO8zTbMNALzqB4umw1qKysd67vUT_wfF5vUnPCn4")
        ("y"
         .
         "YE-BdONgW48YvtN0K2hxqM_84AbbMbjX2Db1DPzafRY"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(124 48 32 226 121 203 90 241 65 132 180 101 60 200 124 29 221 127 73 205 49 205 55 26 232 19 104 29 214 97 125 14)
      #t
      ())
    #(734
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "pJnb9zLkOL4OsIS55q2HndeikEu7AEtAAnlpoXHy1CY")
        ("y"
         .
         "e8M3ocguPF96KSeYe4-uE2JyN9Ig-vtAExI7-9lfC6U"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(172 253 255 86 107 139 85 49 136 105 250 100 111 120 159 128 54 212 11 144 240 252 82 10 226 165 162 117 68 249 98 192)
      #t
      ())
    #(735
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "rc8P-6nLbvDIAxxCkaQ0sY149C5F5iugH76R-Sc_CtE")
        ("y"
         .
         "3fmmZr8AFbIOSRL3D2Ve8huCCHWWqh4vHihlNQ0VkYU"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(92 107 1 207 244 230 206 129 166 48 35 139 93 179 102 46 119 251 136 191 253 222 97 68 58 125 133 84 186 0 30 242)
      #t
      ())
    #(736
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "IXEnJdmAas9U06bIK_k8D-JJJoyp9C7OrBnpOl6rgFY")
        ("y"
         .
         "UDr0ouOyYnlWT-2OdyoEPnVjDk44WZdu3oj_zxb1ynE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(231 40 29 18 183 75 6 238 203 39 62 195 224 216 254 102 62 158 193 213 165 12 43 108 104 236 139 54 147 242 60 76)
      #t
      ())
    #(737
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "HgIXaCS9Meq9zgOpQDx9PCrGMfmw6I2akkcBwbLym4U")
        ("y"
         .
         "zzM1YvPgGIkjdDU2dN6EkPydMEJlmOtgB3kVS68q7Bc"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(128 100 62 216 185 5 42 46 116 106 38 217 23 143 226 204 255 53 237 187 129 246 12 215 128 4 251 141 95 20 58 174)
      #t
      ())
    #(738
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Y-ehrzbWtUCkknaqw_7Jy0Xta6sWfAawQZp3uROZ9hg")
        ("y"
         .
         "Hm_iwacVFj76-G6oseVepXQtawQubL-KzGnJn4JxqQI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(117 135 58 197 68 173 105 211 221 197 201 207 254 56 77 39 94 157 162 148 157 105 130 218 75 153 15 139 242 183 100 116)
      #t
      ())
    #(739
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "HiZatbf3GZRw5TJlPSp7motyiXC4OBN8lpLtBpKJeyo")
        ("y"
         .
         "wdXun8POSf1FCdM8Tc_MGiCmYFKfqevW5q_D1chMcrs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(53 92 159 172 162 156 247 204 150 136 83 238 41 255 230 45 17 39 252 193 220 87 233 221 175 14 15 68 113 70 6 78)
      #t
      ())
    #(740
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "VNKkOUwQn8vTy5iG_sOt1Ruk0uROHVZ25LmPDBNlX8U")
        ("y"
         .
         "8goMphi6ATGi43PzH3Oz9V6RiNRv3bxjh-Mq77nzuhI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(252 23 90 94 241 133 149 182 158 69 190 44 218 138 224 13 156 139 219 239 188 247 246 146 249 28 239 220 86 14 71 34)
      #t
      ())
    #(741
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "k_FFkgf7CcbwqIw5isgNEFKkzTPn7vVofamauXxgJLc")
        ("y"
         .
         "cMYJdXSPDHSaiw-PweIi3cvTOE9taPC2tv9nm0Nc3LE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(70 85 145 70 169 58 174 144 77 188 170 170 7 230 205 27 180 80 241 179 124 131 146 154 153 75 69 121 35 51 213 246)
      #t
      ())
    #(742
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "H6BJoYkrZ5hXxt_wivGdtwy8mbby17xRo0H-edFkf0o")
        ("y"
         .
         "WqnA0bJA5t1CEeMjVCVjSyeK2I_t4DN9Ws8xNlh9hBM"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(198 75 7 17 144 84 163 121 97 192 161 119 21 130 86 8 27 56 176 8 123 48 126 12 173 126 48 215 144 206 176 206)
      #t
      ())
    #(743
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "hOCxktYKv1Mego6IfTZthp4QM6FunH8RZ0WMgTTBD7o")
        ("y"
         .
         "S6LkIMq8Q7bo6GWQ_COD0Xgn3ZmmDCEfGQp0JpEAwUE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(190 168 207 192 190 232 87 28 207 12 82 86 84 239 38 209 252 120 43 178 45 236 207 103 234 78 160 128 61 193 93 175)
      #t
      ())
    #(744
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "L5cHxnEYckER77u78GtiOrL_2SWd3DVPyq-BugH2-ns")
        ("y"
         .
         "JwKlFamlDbHYb9Qq6gg02utivgPQzZAz-EucS1ahnxI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(96 69 29 164 173 254 91 179 147 16 144 105 239 220 132 65 94 200 162 196 41 149 92 191 34 164 52 15 143 196 137 54)
      #t
      ())
    #(745
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "rB-75CKTqfmuEE7i2gsKmzRk1dix6FTfGdPERWr4-aY")
        ("y"
         .
         "EEJ3FwaS-mvyJwWA1W0byBtU9HfYq2w_WEJlCscXbXE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(214 142 116 111 61 67 254 172 95 212 137 141 233 67 220 56 32 90 247 226 99 30 215 50 7 155 191 200 171 82 81 28)
      #t
      ())
    #(746
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "uuEM-T_3ty1u2YUZYC6fA6pAMD-gZ0-z3e59LbHJK7I")
        ("y"
         .
         "XTZ2iP3l4ILwl4VaDArcMF3Wz0b1DKdYWbskO3AklgU"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(40 218 234 173 198 9 56 109 119 13 255 76 113 32 178 168 124 171 62 33 253 184 166 228 220 18 64 165 29 18 229 92)
      #t
      ())
    #(747
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "7bQojPVWdnPVChzZ5r6kUxeCPzA4P2DZvDue5CrCmHE")
        ("y"
         .
         "9NHSKvcYfI3huZKkBGxBm4Ac3lfWONMPLhrEk1MReiA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(187 65 16 183 52 200 239 138 8 187 96 17 172 179 92 189 169 174 142 46 246 196 208 134 37 118 166 135 146 102 123 185)
      #t
      ())
    #(748
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "EyM-gPWawrWXN-h4d3gqswJ8SQ34rAvz8-8WM4cu7FQ")
        ("y"
         .
         "AWL5VFNOrbG06pXFfUChAhTlt0buaqQZTtKyASty-X0"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(226 92 80 3 124 161 145 56 81 185 117 135 82 101 159 182 28 2 210 167 198 182 170 226 155 218 48 25 7 217 159 93)
      #t
      ())
    #(749
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "PNFPfkt3lhW8fM7kfn8rBzlL-PmFAyY0EaVJJkqPzxk")
        ("y"
         .
         "YE-BdONgW48YvtN0K2hxqM_84AbbMbjX2Db1DPzafRY"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(173 37 159 1 233 83 38 63 64 163 155 20 165 56 208 118 113 12 25 32 122 249 54 254 171 223 3 189 167 240 103 165)
      #t
      ())
    #(750
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lGwngohhaqNHkMoZNobnRdPVhwKGbd8elVUHEam_vbg")
        ("y"
         .
         "e8M3ocguPF96KSeYe4-uE2JyN9Ig-vtAExI7-9lfC6U"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(94 198 2 90 199 178 92 15 9 95 63 222 227 226 229 8 189 20 55 185 112 92 37 67 192 229 175 28 29 54 63 253)
      #t
      ())
    #(751
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "fxlQNf6ywEqbFJuy7TxcRY6V5_fEGMSgfqYQfk4yRVo")
        ("y"
         .
         "3fmmZr8AFbIOSRL3D2Ve8huCCHWWqh4vHihlNQ0VkYU"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(162 249 58 132 87 74 38 180 56 128 205 230 237 68 12 127 124 199 44 146 80 77 82 113 153 154 138 120 255 227 73 29)
      #t
      ())
    #(752
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "QIVYROBDA4Q6JLAXB1RNG7-XZzJm4D13-_gNi2Qhm9g")
        ("y"
         .
         "UDr0ouOyYnlWT-2OdyoEPnVjDk44WZdu3oj_zxb1ynE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(141 12 219 73 119 186 118 97 212 16 54 174 183 165 242 221 32 119 22 213 215 110 235 38 98 144 67 197 89 236 41 0)
      #t
      ())
    #(753
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Is2z7kfxSzsMDIwlb7IueRJrQ2osn_Y1plFRoPD_sb8")
        ("y"
         .
         "zzM1YvPgGIkjdDU2dN6EkPydMEJlmOtgB3kVS68q7Bc"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(222 253 228 170 72 248 155 3 246 35 234 31 148 111 26 169 56 197 170 184 121 202 99 25 89 105 38 240 133 87 142 220)
      #t
      ())
    #(754
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "K3vs1wZuIvEh588SPUjFRFA3xadW7zFKZqcAFjbudc8")
        ("y"
         .
         "Hm_iwacVFj76-G6oseVepXQtawQubL-KzGnJn4JxqQI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(175 224 191 237 105 166 0 22 56 101 64 97 39 168 151 43 97 50 50 170 76 147 58 6 181 165 181 188 255 21 150 248)
      #t
      ())
    #(755
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "u42kp27j0cSzNHe8hmPe8WehJsQirUf2wvi1OcaAiTY")
        ("y"
         .
         "wdXun8POSf1FCdM8Tc_MGiCmYFKfqevW5q_D1chMcrs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(244 155 202 122 106 82 86 221 247 18 119 89 23 195 14 72 115 21 52 105 186 225 47 213 197 87 16 49 219 123 18 5)
      #t
      ())
    #(756
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Cgw3ZkgjpQBdZZ98c8OeoXLIYpacgeRPNsiefCZeyKg")
        ("y"
         .
         "8goMphi6ATGi43PzH3Oz9V6RiNRv3bxjh-Mq77nzuhI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(156 136 182 17 183 249 170 211 63 171 176 156 255 97 139 177 202 111 185 4 162 137 177 72 29 163 209 228 231 37 137 228)
      #t
      ())
    #(757
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "R8M_b3jTzZlx7MUOfirJR_jBED-cXwghN5vQatj8pFY")
        ("y"
         .
         "cMYJdXSPDHSaiw-PweIi3cvTOE9taPC2tv9nm0Nc3LE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(66 246 52 192 108 74 14 126 149 109 182 232 102 102 96 61 38 55 76 199 75 17 2 111 3 24 209 162 86 129 167 18)
      #t
      ())
    #(758
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "tZ0Yq4sPndM0hPQ8P2hgIpumpMJaYc0KrKI7dtYFZs8")
        ("y"
         .
         "WqnA0bJA5t1CEeMjVCVjSyeK2I_t4DN9Ws8xNlh9hBM"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(226 206 185 70 231 153 63 39 164 50 122 189 246 29 79 6 87 126 137 198 59 98 162 74 239 189 144 87 16 209 134 105)
      #t
      ())
    #(759
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "lPRgGyRNOm6mmW-iRDZPeUOZ4P9DFhV9tgIyIvwNkL4")
        ("y"
         .
         "S6LkIMq8Q7bo6GWQ_COD0Xgn3ZmmDCEfGQp0JpEAwUE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(113 99 122 93 162 65 42 146 31 22 54 198 154 110 232 16 131 238 43 14 19 118 106 209 34 121 30 246 247 113 137 109)
      #t
      ())
    #(760
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "nowRWxrIfZhu4bUGuGpOe46gQapqY9bsgOwPDPac-z8")
        ("y"
         .
         "JwKlFamlDbHYb9Qq6gg02utivgPQzZAz-EucS1ahnxI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(189 38 94 211 7 140 168 199 120 143 89 65 135 201 108 103 90 166 35 236 208 27 252 173 98 215 106 120 129 51 79 99)
      #t
      ())
    #(761
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "7sd2tSuUFB_IGdS2sS0o5zVVtVYFB6un328EhACN6R8")
        ("y"
         .
         "EEJ3FwaS-mvyJwWA1W0byBtU9HfYq2w_WEJlCscXbXE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(141 7 63 197 146 251 122 166 247 185 8 237 7 20 138 167 190 90 19 92 75 52 62 190 41 81 152 203 167 142 113 206)
      #t
      ())
    #(762
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "r_RqOI5a_CIKjux6Sa-dJFOEo68eC0B7RSH06S0S3Os")
        ("y"
         .
         "XTZ2iP3l4ILwl4VaDArcMF3Wz0b1DKdYWbskO3AklgU"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(162 109 105 142 70 19 89 90 166 28 142 41 7 213 36 29 109 20 144 151 55 223 89 137 88 65 208 119 39 191 19 72)
      #t
      ())
    #(763
      "point with coordinate y = 1 in left to right addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "6AfkPZbzcBqaXBPRInSQhBcPzTalhqRGyfy0YA7t5P0")
        ("y"
         .
         "9NHSKvcYfI3huZKkBGxBm4Ac3lfWONMPLhrEk1MReiA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(168 237 198 249 175 107 247 65 34 193 28 161 165 10 251 196 163 196 152 123 208 209 247 50 132 210 193 55 30 97 52 5)
      #t
      ())
    #(764
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "eYhopWkW00Hn1vljWa42WINuIhRZ9Pe3tjaU3hil6SQ")
        ("y"
         .
         "dxP9sDqN6MbSnKOKn7qoLl4Cvq0vnuxptkRLetsFMzs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(23 150 61 224 120 153 110 184 80 60 124 195 225 162 213 20 125 127 11 251 37 26 2 11 67 146 3 48 99 88 124 141)
      #t
      ())
    #(765
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_0GZCdiozgqUFgUfTiViCMHcA1WBpTMS1WYTfiIQTpg")
        ("y"
         .
         "d0IasB4A6DhBuUba5btaI5c9qpj-GoFyiDq8vtztcCE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(6 39 153 161 149 69 211 27 62 215 34 83 188 222 89 118 42 166 16 74 136 172 94 47 182 137 38 176 247 20 102 152)
      #t
      ())
    #(766
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "i0gRnXCJ07lc0ur4yFWE-o9eVsTEzO5wN9dM2_iOVxc")
        ("y"
         .
         "FMGqxfC_G0ikq88dkpG5qHdqAEOAVGpaHB8pRpD2GWk"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(159 66 221 143 206 19 248 16 59 59 43 193 94 97 36 46 104 32 254 19 37 162 14 244 96 254 100 217 235 18 178 49)
      #t
      ())
    #(767
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "4oiBGTebWyFRvXiFBd7x1r14YylDHK85cF2cv5akLqQ")
        ("y"
         .
         "O7cyiDnSrsrGSxzbGC8IrcyqwyftAImHoQ7clzJBPO0"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(209 178 4 229 45 31 172 109 80 65 50 199 108 162 51 200 126 55 125 204 121 200 147 201 112 221 187 159 135 178 127 160)
      #t
      ())
    #(768
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "bcw5cb0gkT1ZqR8g2RL1bQfn8BQga-9KZT3f5dEoQsM")
        ("y"
         .
         "m1Gxe3bqbME37r2TyBHmNtiuJscNBkZQ9yBahl0Bpu4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(200 214 189 40 193 230 90 231 199 165 222 190 103 167 223 175 146 180 41 237 227 104 239 201 218 125 87 138 83 155 112 84)
      #t
      ())
    #(769
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "fr6kWFRWmh9-prlbgta-_vv2KW68h8gQtsupPAwSILI")
        ("y"
         .
         "Pxh0-gimk7CGZD7yHrWddVYtqUItE9mjmwsX4kGwTTI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(13 31 144 92 199 71 32 189 230 122 232 79 88 39 40 88 140 117 68 76 39 61 174 65 6 250 32 209 214 148 100 48)
      #t
      ())
    #(770
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "zqtZN5ANNPqIN403H0rKp8aiAothQyE0E_FrotxxR4c")
        ("y"
         .
         "dxP9sDqN6MbSnKOKn7qoLl4Cvq0vnuxptkRLetsFMzs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(63 1 78 48 145 146 88 143 168 62 71 212 172 150 133 210 4 18 4 226 234 246 51 161 49 40 18 229 26 231 76 189)
      #t
      ())
    #(771
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "pP_qXiX3Xk9onIEISjXBIg6Oa5FMSC9KLo-Tz_ymlkc")
        ("y"
         .
         "d0IasB4A6DhBuUba5btaI5c9qpj-GoFyiDq8vtztcCE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(104 180 4 213 86 200 32 4 198 196 187 164 81 142 192 11 29 79 17 97 202 254 108 137 174 184 73 74 155 160 157 181)
      #t
      ())
    #(772
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "3ogJ6g7M4dJKBDFClRA4Om9uWhxRzqMtgwxsNTBCYD4")
        ("y"
         .
         "FMGqxfC_G0ikq88dkpG5qHdqAEOAVGpaHB8pRpD2GWk"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(195 49 173 231 164 87 223 127 18 162 245 196 61 126 169 72 108 21 99 184 28 216 160 242 63 146 60 26 159 166 18 227)
      #t
      ())
    #(773
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "VmIJ8XTWv3lyC3Dtsn5RNQvusrC80IO7rnIU9xz4JNQ")
        ("y"
         .
         "O7cyiDnSrsrGSxzbGC8IrcyqwyftAImHoQ7clzJBPO0"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(23 181 199 163 17 238 169 210 171 117 113 248 185 248 72 212 112 89 151 207 62 175 155 220 190 14 52 166 112 248 31 69)
      #t
      ())
    #(774
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "zDGBwBJxN1Ns7slP1FmWZX33Lg-XxEudrRR2POUG6dw")
        ("y"
         .
         "m1Gxe3bqbME37r2TyBHmNtiuJscNBkZQ9yBahl0Bpu4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(47 14 78 204 188 69 24 172 229 88 224 102 4 249 191 244 120 127 91 1 148 55 181 33 149 236 182 184 33 145 166 174)
      #t
      ())
    #(775
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "1wUqHur8DnjXnn8mADqgpAkofPR2AH3yjSgbFCvhoOI")
        ("y"
         .
         "Pxh0-gimk7CGZD7yHrWddVYtqUItE9mjmwsX4kGwTTI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(116 148 216 100 203 110 169 197 217 130 212 10 95 16 55 0 208 45 201 130 99 119 83 207 199 216 175 225 190 175 255 112)
      #t
      ())
    #(776
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "t8w-Iwbb98OP8Xllhwb-_7Xv22BEx-cUNdf_fQrox7M")
        ("y"
         .
         "dxP9sDqN6MbSnKOKn7qoLl4Cvq0vnuxptkRLetsFMzs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(169 104 115 238 245 212 56 184 7 133 59 103 113 198 165 25 126 110 239 33 239 239 202 83 139 69 233 233 129 192 50 229)
      #t
      ())
    #(777
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "W758mAFf06YDTXnYZ6Tc1S-VkRkyEp2i_ApYr-FJE38")
        ("y"
         .
         "d0IasB4A6DhBuUba5btaI5c9qpj-GoFyiDq8vtztcCE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(145 36 97 137 19 242 12 223 250 100 34 7 241 146 230 126 184 10 222 83 172 85 53 70 154 190 144 3 109 74 247 226)
      #t
      ())
    #(778
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "li_keICpSnRZKOPEoppCywEzTx7pZG5iRRxG7NcvQQk")
        ("y"
         .
         "FMGqxfC_G0ikq88dkpG5qHdqAEOAVGpaHB8pRpD2GWk"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(157 139 116 136 141 148 40 112 178 33 222 122 100 32 50 137 43 201 158 52 189 133 80 25 95 111 95 9 117 71 51 74)
      #t
      ())
    #(779
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "xxV09VON5WU8NxaNR6K89DaY6iYAEs0K4TBOR0xjpOY")
        ("y"
         .
         "O7cyiDnSrsrGSxzbGC8IrcyqwyftAImHoQ7clzJBPO0"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(22 152 51 119 192 241 169 192 4 73 91 63 217 101 131 99 17 110 234 100 71 135 208 89 209 20 15 185 7 85 93 74)
      #t
      ())
    #(780
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "xgJEzjBuN285aBePUpN0LXog4dxHz8UX7a2p20nQy78")
        ("y"
         .
         "m1Gxe3bqbME37r2TyBHmNtiuJscNBkZQ9yBahl0Bpu4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(8 26 244 10 129 212 140 107 83 1 64 219 147 94 96 91 244 204 123 16 136 95 91 20 143 149 241 188 138 210 229 45)
      #t
      ())
    #(781
      "point with coordinate y = 1 in precomputation or right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "qjwxiMCtV2epusd-fO6gXPrhWZzNd7n8vAw7rcgMNso")
        ("y"
         .
         "Pxh0-gimk7CGZD7yHrWddVYtqUItE9mjmwsX4kGwTTI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(126 75 151 62 109 74 53 124 64 2 67 166 72 200 160 166 163 92 242 49 117 74 253 239 49 45 47 75 106 187 152 143)
      #t
      ())
    #(782
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "LM6N3-SCfcAw3fOPmYs_LtXgYh0LOAVmba9IyMMedeU")
        ("y"
         .
         "GY2e9Olztr3r4RmjX6roYZGs11jB7YrMrx5watVdg9c"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(15 2 53 218 42 6 200 212 8 194 113 81 243 241 83 66 237 140 25 69 170 248 78 209 73 147 120 109 106 197 245 112)
      #t
      ())
    #(783
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "FL_D5aRraYgamjRtlYlEGGFO2RR2od3OSGdrfLq5ugI")
        ("y"
         .
         "8zTWTyyvVhsGO8H3iJ6TcwKkVf9oXYrlfLJEShfa0Gg"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(86 34 194 251 232 175 90 214 206 247 42 1 190 24 110 85 72 71 87 97 6 248 151 151 114 250 86 17 77 17 96 171)
      #t
      ())
    #(784
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "vUQvpaKo1y4T5E_SIiyFoAbwM3XgIRsnL1VQUrA9t1A")
        ("y"
         .
         "vjRXN_fGtecOl9n-ncTKlPsYX0udKgDghsHUcnOzNgI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(187 149 224 208 251 170 216 108 91 216 123 149 148 108 119 255 29 101 50 42 23 92 207 22 65 145 2 192 161 127 90 114)
      #t
      ())
    #(785
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "DXo_9Jvaalh-0HaRRQQlqgLSU7pXOhathsYa9BLdPHc")
        ("y"
         .
         "C207nlcLoASHfJpp5IH-IV3gOnASYwWkUoJuZtm1WD4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(69 16 104 60 123 250 37 31 12 181 107 186 126 10 183 77 144 245 226 202 1 233 30 124 169 147 18 204 255 45 144 182)
      #t
      ())
    #(786
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "vepdKjrd598ug5_2P2JTSz8nyxkbtU36HTnL_3E7qe0")
        ("y"
         .
         "MH2PHQLG8HFGZV5jg7DvMDW-5wZ8M2_bkTZeGXqXthY"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(2 84 133 20 44 161 206 215 82 40 159 119 33 48 252 16 199 90 69 8 196 107 255 222 249 41 10 211 231 186 249 202)
      #t
      ())
    #(787
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "1MBj48A29HyS9vVHCiaoNeGiRQWxTRspJ5BioWz29Ik")
        ("y"
         .
         "GY2e9Olztr3r4RmjX6roYZGs11jB7YrMrx5watVdg9c"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(144 103 147 33 80 114 73 101 170 71 156 30 241 190 85 84 75 237 159 169 69 0 163 182 120 135 237 145 174 59 129 229)
      #t
      ())
    #(788
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "PLnweZd1aFnpuahbaB-lDuIDV_U1wbMRxGN9Frdrnr8")
        ("y"
         .
         "8zTWTyyvVhsGO8H3iJ6TcwKkVf9oXYrlfLJEShfa0Gg"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(248 8 74 137 173 204 220 58 239 137 229 9 26 15 7 214 22 10 102 203 149 117 36 17 0 193 211 155 240 84 154 226)
      #t
      ())
    #(789
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "eTQS_2NsCKLQ9tYMxgjpqQmDSaJQH5HJX2kgELwSOLI")
        ("y"
         .
         "vjRXN_fGtecOl9n-ncTKlPsYX0udKgDghsHUcnOzNgI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(68 98 85 140 137 144 33 23 5 28 178 197 153 173 102 240 8 135 181 76 174 61 169 192 77 49 122 91 42 251 70 59)
      #t
      ())
    #(790
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "vR6whJ4uahPVS3ZRjxG6h3XC12NNhRUlNLx8OvQWHvo")
        ("y"
         .
         "C207nlcLoASHfJpp5IH-IV3gOnASYwWkUoJuZtm1WD4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(48 180 116 26 100 248 125 40 236 0 41 189 25 107 90 116 85 95 44 154 151 106 70 214 40 87 36 116 70 106 99 29)
      #t
      ())
    #(791
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Yks7S6mTqLk4ElaJ9s91c5LuOQ0UqQ_qbblEtajeuNA")
        ("y"
         .
         "MH2PHQLG8HFGZV5jg7DvMDW-5wZ8M2_bkTZeGXqXthY"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(58 252 4 172 146 17 126 80 176 145 59 9 219 187 78 108 120 12 5 21 0 32 31 173 81 43 121 8 11 255 57 226)
      #t
      ())
    #(792
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_nEOPFtGjcM8KxcpXE4Ym0h9WN1Det9wasBUk8_qjfA")
        ("y"
         .
         "GY2e9Olztr3r4RmjX6roYZGs11jB7YrMrx5watVdg9c"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(96 150 55 4 133 134 237 198 76 245 242 143 26 80 87 104 198 134 71 17 16 7 13 120 61 228 153 255 230 254 132 218)
      #t
      ())
    #(793
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "roZLoMQfLh37rCM3AlcW2LytzvZTnG8f8zUXa43ao24")
        ("y"
         .
         "8zTWTyyvVhsGO8H3iJ6TcwKkVf9oXYrlfLJEShfa0Gg"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(177 212 242 122 105 131 200 238 65 126 240 245 39 216 137 212 161 174 65 211 99 146 68 87 140 67 214 80 194 153 252 209)
      #t
      ())
    #(794
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "yYe9WvnrIC8bJNohF8qQtu-MgufPv1MPcUGPmpOwCFw")
        ("y"
         .
         "vjRXN_fGtecOl9n-ncTKlPsYX0udKgDghsHUcnOzNgI"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(0 7 201 162 122 197 6 124 159 10 209 164 209 230 33 16 218 19 24 137 58 101 135 41 113 61 130 227 51 133 91 130)
      #t
      ())
    #(795
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "NWcPhsX3K5Or5BMdK-ofzodq1OJbQNQtRH1oz_kMoL4")
        ("y"
         .
         "C207nlcLoASHfJpp5IH-IV3gOnASYwWkUoJuZtm1WD4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(138 59 35 169 31 13 93 184 7 74 106 136 104 137 238 62 25 170 240 155 102 172 154 173 46 21 200 189 186 104 8 92)
      #t
      ())
    #(796
      "point with coordinate y = 1 in right to left addition chain"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "38pnihuOb2eZagl_yc43QS3p-9nPoaIbdQzvSOXllaE")
        ("y"
         .
         "MH2PHQLG8HFGZV5jg7DvMDW-5wZ8M2_bkTZeGXqXthY"))
      #(("crv" . "P-256K")
        ("d"
         .
         "wXgdhsrCwFK4ZfIo5kvRzkM8eMp9_KnouBBHPizhfaU")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "T1kiFy_kxAFvsDejK_kdoPg-hKY6OxSgxUkcmVytx10")
        ("y"
         .
         "8Wl0jjcIwEUZ0KmUKYL8OuQJJFeGEBhRkr4kGK3K3F0"))
      #vu8(194 175 118 63 65 76 178 215 253 70 37 127 3 19 181 130 192 153 181 226 59 115 224 115 181 171 124 35 12 69 200 131)
      #t
      ())
    #(797
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "-TCKAZJYwxBJNE-F-J1SKbUxyEWDb5mwhgHxE7zgNvk")
        ("y"
         .
         "OI97D2Mt6BQP4zfmKjfzVmUAqZk0wiMbbLn9dYS45nI"))
      #vu8(52 0 86 148 227 202 192 147 50 170 66 128 126 58 253 195 179 179 188 124 123 232 135 209 249 141 118 119 140 85 207 215)
      #t
      ())
    #(798
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "AAAAAP____________________________________8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "pJAKfBVOYR-__x6-BGEVzzH8fjtVOMAsrZQTQL9z14k")
        ("y"
         .
         "NwDMVH5S6Ur1FHzunRDXdG0gZ9XbJRN3lzvW597oKr8"))
      #vu8(88 65 172 211 207 242 214 40 97 187 225 16 132 115 128 6 214 140 207 53 172 174 97 94 233 82 71 38 233 61 13 165)
      #t
      ())
    #(799
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "AQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "jCipe_gpi8DSPYx0lFKjLmlLZeMKlHKjlUqzD-UyTKo")
        ("y"
         .
         "QKMEY6MwUZM3j-3zH3zA63rnhPBFHLlFnnHcc8vvlII"))
      #vu8(67 72 228 203 163 113 234 208 57 130 1 138 188 154 172 236 174 191 214 54 221 168 46 96 159 210 152 148 127 144 125 232)
      #t
      ())
    #(800
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "f_________________________________________8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Nw6_7UcxeBWf0Iw_e8B-EjAXkvvSUVVKgCmO_GZsZR0")
        ("y"
         .
         "rQi3UWHFQuVQO3d2JcKWue-FRVdWun1YK8PACWXepKI"))
      #vu8(229 98 33 194 176 220 51 185 139 144 223 211 35 154 44 12 177 228 173 3 153 163 170 239 63 157 71 251 16 61 174 240)
      #t
      ())
    #(801
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "gAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "sjeQpCvmPhslGtbJT97wcnHsCq2jHbbD6L0yBD-L44Q")
        ("y"
         .
         "_GtpSRnVXtvo1Q-IqoH5RRfwBPQUnstY0QpHPesZiA4"))
      #vu8(91 52 162 155 28 77 220 178 16 17 98 211 75 237 159 7 2 54 31 229 175 80 93 243 21 239 247 190 253 14 71 25)
      #t
      ())
    #(802
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "_____________________rqu3OavSKA6v9JejNA2QUE")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "MyLUASQ8TiWCohR8EE1uy_d00WPbD15TE7fg50LQ5r0")
        ("y"
         .
         "qRj4aBaZsQpAT-ZDsiUGSNf6CcFdeMUJ2wxdFZPXSY8"))
      #vu8(206 206 82 27 139 90 50 187 238 56 147 107 167 214 69 130 79 35 142 86 23 1 163 134 251 136 142 1 13 181 75 47)
      #t
      ())
    #(803
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "_____________________rqu3OavSKA7v8JejNA2QUE")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "jnvNC9NZg6dxnMp3ZMqQZ3m1OgQ6m4vK7_lZ9DrYYEc")
        ("y"
         .
         "70iI9NXCW0xr_O-981auuoYXcdG4ApdMFe_7gHufxQU"))
      #vu8(130 149 33 183 157 113 245 1 30 7 151 86 184 81 160 213 200 53 87 134 97 137 166 37 140 30 120 161 112 12 105 4)
      #t
      ())
    #(804
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "_____________________rqu3OavSKA7v8pejNA2QUE")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "j1BvC2wLbppXp_Ntlwyk40fLySFGInZCy-eB2fU2LTM")
        ("y"
         .
         "uWBqotUFno5qzzq9sOPMl7cwbaK8RHFQz7eC8ngF1_A"))
      #vu8(140 89 52 121 53 5 166 161 248 77 65 40 51 65 104 12 73 35 241 244 213 98 152 154 17 204 98 111 234 94 218 90)
      #t
      ())
    #(805
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "_____________________rqu3OavSKA7v9Jei9A2QUE")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "EA9E2mlucWcnkdCgm3veRZ8SFaKbPAO_79eDWzmkjbA")
        ("y"
         .
         "MiYezm1f9IjRNwzP8_b5mUgAtecArmpT8EKjKNQ5oiY"))
      #vu8(53 108 174 231 231 238 224 49 161 94 84 195 165 196 231 47 156 116 187 40 124 230 1 97 158 248 94 185 108 40 148 82)
      #t
      ())
    #(806
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "_____________________rqu3OavSKA7v9JejNA2QMM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "bfe1p6EmphEuHgugGtGg-J8FXdPBx-UzaTitMsSUsxk")
        ("y"
         .
         "YdpmS99n2spNbzC6zaUPS1fhBZS-tafBFPat9GfgHP0"))
      #vu8(9 199 51 125 246 194 179 94 223 58 33 56 37 17 204 90 221 26 113 168 76 191 141 51 150 165 190 84 141 146 250 103)
      #t
      ("AddSubChain"))
    #(807
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "_____________________rqu3OavSKA7v9JejNA2QQM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "EIRDuUjRVTWEonEzP3-9BDxNZqkXBu3svwf2iUwE8pk")
        ("y"
         .
         "sYSiVFywQwYPqq3yskc7YCn9fSzVIDWqpPu_wUan4ZA"))
      #vu8(209 108 174 221 37 121 54 102 249 226 111 83 49 56 33 6 245 64 149 179 210 13 64 199 69 182 140 167 108 14 105 131)
      #t
      ("AddSubChain"))
    #(808
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "_____________________rqu3OavSKA7v9JejNA2QSM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "bSsIXp44LtELafwxGgP4ZBzP_yFXTeCSdROknZpoigA")
        ("y"
         .
         "U0fRRsz2UuM4xiIFzJ-1fIidx1X0KgDbckU7hOgMczQ"))
      #vu8(184 174 30 33 216 179 76 228 202 255 237 113 103 162 104 104 236 128 167 212 166 169 139 99 157 77 5 205 34 101 4 222)
      #t
      ("AddSubChain"))
    #(809
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "_____________________rqu3OavSKA7v9JejNA2QTM")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "SZ_fnolecZz9ZOZ_B9OOMiaqe2NniUnm5JskGmDoI-Q")
        ("y"
         .
         "NT0JO0qxeq5vD7sbWEwrm7m9hj2FwGpDOaC_KvxevNQ"))
      #vu8(2 119 99 21 254 20 122 54 164 176 152 116 146 182 80 58 205 234 96 249 38 69 14 94 221 185 248 143 200 33 120 211)
      #t
      ("AddSubChain"))
    #(810
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "_____________________rqu3OavSKA7v9JejNA2QTs")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "__l71XVe7qQgRToUNVI104L2Ry-FaKGLLwV6FGApdVY")
        ("y"
         .
         "Ue2IhVMESd8MQWn-gLo6nyF_DwmucBtfw3jzyE-KCZg"))
      #vu8(57 136 201 199 5 10 40 121 73 52 229 189 103 98 155 85 109 151 164 133 141 34 129 40 53 244 163 125 202 53 25 67)
      #t
      ("AddSubChain"))
    #(811
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "_____________________rqu3OavSKA7v9JejNA2QT4")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "-TCKAZJYwxBJNE-F-J1SKbUxyEWDb5mwhgHxE7zgNvk")
        ("y"
         .
         "x3CE8JzSF-vwHMgZ1cgMqZr_VmbLPdzkk0YCiXtHFb0"))
      #vu8(52 0 86 148 227 202 192 147 50 170 66 128 126 58 253 195 179 179 188 124 123 232 135 209 249 141 118 119 140 85 207 215)
      #t
      ())
    #(812
      "edge case private key"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "Mr3ZeOtisfNppW0JSauFUaetUn2WAuiRzkV1hsKoVp4")
        ("y"
         .
         "mB5n-uBTsD_DPhopHwo761j86y6FuxIF2s7hIy39MWs"))
      #(("crv" . "P-256K")
        ("d"
         .
         "_____________________rqu3OavSKA7v9JejNA2QT8")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "xgR_lEHtfW0wRUBulcB82Fx3jkuM7zynq6wJuVxwnuU")
        ("y"
         .
         "5R6XAVnCPMZcOnvmuZMVEQgJzZrNmS8e3JvOVa8wFwU"))
      #vu8(75 82 37 125 139 59 163 135 121 127 223 122 117 47 25 93 220 79 125 118 38 61 230 29 13 82 165 236 20 163 108 191)
      #t
      ("AddSubChain"))
    #(813
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(814
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(815
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "_____________________________________v___C4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(816
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "_____________________________________v___C8"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(817
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(818
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(819
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE")
        ("y"
         .
         "_____________________________________v___C4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(820
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE")
        ("y"
         .
         "_____________________________________v___C8"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(821
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____________________________________v___C4")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(822
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____________________________________v___C4")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(823
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____________________________________v___C4")
        ("y"
         .
         "_____________________________________v___C4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(824
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____________________________________v___C4")
        ("y"
         .
         "_____________________________________v___C8"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(825
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____________________________________v___C8")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(826
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____________________________________v___C8")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(827
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____________________________________v___C8")
        ("y"
         .
         "_____________________________________v___C4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(828
      "point is not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____________________________________v___C8")
        ("y"
         .
         "_____________________________________v___C8"))
      #(("crv" . "P-256K")
        ("d"
         .
         "xsr7dOKlDIOz0jLEWFI39E1MVDPEs_UM6Xjmrto6T10")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "WXjzLk1W8-x2M-bAMCcw5dpeJGoqVZVmqDUYzSwWnDA")
        ("y"
         .
         "LmTWFQtxMTMb_OYxWBUOheh1YF6Mk6EI8vGaKvkvN6c"))
      #vu8()
      #f
      ())
    #(829
      "public key has invalid point of order 2 on secp256r1.  The point of the public key is a valid on secp256k1."
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "_____wAAAAEAAAAAAAAAAAAAAAD_______________8")
        ("y"
         .
         "MtmODXfdDlQ3cOyZTAroN-e7NusdkQtYoUoqCNwYL4M"))
      #(("crv" . "P-256K")
        ("d"
         .
         "OyUSnzQQ7InMbcU5_XYBhzumq_cqbQI_GqkEF2VDDuY")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "aIhcjAW8Rf7cnW7lalJlX8E_QDXJL22taiwkqt1Vl7A")
        ("y"
         .
         "Tg0OyxYgsShy0PHK_uLjO3Ij7XLXd4RtTIVbKIZ8lVQ"))
      #vu8(29 63 194 178 228 139 62 150 198 50 51 128 250 219 70 120 37 230 159 91 144 120 169 224 33 115 180 119 188 35 44 193)
      #f
      ())
    #(830
      "public point not on curve"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "ScJI7cZZ4YSCtxBXSKS5XTpGlSpbpy2g1wLcl6ZOmXk")
        ("y"
         .
         "nYz_elxLkl5DYOziXM8wfXqacGMoa70W72TGX1RnV-Q"))
      #(("crv" . "P-256K")
        ("d"
         .
         "z-de52QZeqdzKlR4VWtHiJhCPSvA5ISm67NnSmA2pl0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "tQ40JEarfvpswtbKrZT5Pc9wsezCghgTT5indpCQbHA")
        ("y"
         .
         "eCtq8YnoGsxN6V58f6lVsEdae0zSW8ax3bzWF3p3NCk"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(831
      "public point = (0,0)"
      #(("crv" . "P-256K")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        ("y"
         .
         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
      #(("crv" . "P-256K")
        ("d"
         .
         "z-de52QZeqdzKlR4VWtHiJhCPSvA5ISm67NnSmA2pl0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "tQ40JEarfvpswtbKrZT5Pc9wsezCghgTT5indpCQbHA")
        ("y"
         .
         "eCtq8YnoGsxN6V58f6lVsEdae0zSW8ax3bzWF3p3NCk"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(832
      "using secp256r1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "y_ZgZZWj7lD5_OqieYwnQMglQFFrTlp9Nh_yTp3RU2Q")
        ("y"
         .
         "5UCLLmefnVMQ0faJOzbOFrSlB1CRdfy1KupTt4FVazk"))
      #(("crv" . "P-256K")
        ("d"
         .
         "z-de52QZeqdzKlR4VWtHiJhCPSvA5ISm67NnSmA2pl0")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "tQ40JEarfvpswtbKrZT5Pc9wsezCghgTT5indpCQbHA")
        ("y"
         .
         "eCtq8YnoGsxN6V58f6lVsEdae0zSW8ax3bzWF3p3NCk"))
      #vu8()
      #f
      ("InvalidPublic"))
    #(833
      "Public key uses wrong curve: secp256r1"
      #(("crv" . "P-256")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "BjcoUlhAN3Iqf5v6rVZhrLYjFi1F9wpVLGF_QIDoc6o")
        ("y"
         .
         "Q2CSdd_23KqhIqdF0PFUaB-cdyaGe0PnUjt_WrXqlj4"))
      #(("crv" . "P-256K")
        ("d"
         .
         "2vogng-BEZpK-j8bxG4veUc1TjcnxgiwXElQsQOGZDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "eWx5IWNXTk94H28EIIraU7gCUaxE1Pgcm4_iNpoCB6w")
        ("y"
         .
         "IaQvFiWjt8RdTus17eL1jkDwIA1JruExim_e7kseq_c"))
      #vu8()
      #f
      ())
    #(834
      "Public key uses wrong curve: secp384r1"
      #(("crv" . "P-384")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "DvWARzHZGPA3UG7gC4YCuHfH1Qn_osCEeobnotNYunyYHCp0siQBrGFTB6besnVA")
        ("y"
         .
         "L6bIIYwzdPipF1LS7_a9FK2MrlltLzfa6K7sCFdg7fT9qafPcCU4mKVBg0aQcqVh"))
      #(("crv" . "P-256K")
        ("d"
         .
         "2vogng-BEZpK-j8bxG4veUc1TjcnxgiwXElQsQOGZDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "eWx5IWNXTk94H28EIIraU7gCUaxE1Pgcm4_iNpoCB6w")
        ("y"
         .
         "IaQvFiWjt8RdTus17eL1jkDwIA1JruExim_e7kseq_c"))
      #vu8()
      #f
      ())
    #(835
      "Public key uses wrong curve: secp521r1"
      #(("crv" . "P-521")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "AJIdpXEQ2ybHg4pp1XT8mFiMXAenkss3n0ZmTMdzweH2-hYUhmd0jt4jLRofHOp_FSxdWGFyrL6qSEFry9cLsn8P")
        ("y"
         .
         "AbRHfhrnS_TwkxhKnybxA3Esz2zrRaBQWxkWBtiX7a-HKzfw-QqTMACoD8MgcEgyPBaIOj1nqQqni8ycXljXhLm5"))
      #(("crv" . "P-256K")
        ("d"
         .
         "2vogng-BEZpK-j8bxG4veUc1TjcnxgiwXElQsQOGZDo")
        ("kid" . "none")
        ("kty" . "EC")
        ("x"
         .
         "eWx5IWNXTk94H28EIIraU7gCUaxE1Pgcm4_iNpoCB6w")
        ("y"
         .
         "IaQvFiWjt8RdTus17eL1jkDwIA1JruExim_e7kseq_c"))
      #vu8()
      #f
      ())))
