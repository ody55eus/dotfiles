(list (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (branch "master")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
       (name 'acg)
       (url "ssh://git@gitlab.ody5.de:223/acg/guix.git")
       (branch "main")
       (introduction
        (make-channel-introduction
         "83fe1e90f7db4f61b1390fcbffba3bf418c588c3"
         (openpgp-fingerprint
          "885B 941B 8221 3321 6D96 0E4C DE2A D6CF 2474 B880"))))
      (channel
       (name 'dinos)
       (url "https://gitlab.ody5.de/ody55eus/dinos.git")
       (branch "main")
       (introduction
        (make-channel-introduction
         "e802e65a76e49435ded5ef909120b2b84f2e262b"
         (openpgp-fingerprint
          "885B 941B 8221 3321 6D96 0E4C DE2A D6CF 2474 B880"))))
      (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       ;; Enable signature verification:
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      ;; (channel
      ;;   (name 'rde)
      ;;   (url "https://git.sr.ht/~abcdw/rde")
      ;;   (introduction
      ;;    (make-channel-introduction
      ;;     "257cebd587b66e4d865b3537a9a88cccd7107c95"
      ;;     (openpgp-fingerprint
      ;;      "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
      )
