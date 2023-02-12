(use-modules (guix channels))

(list (channel
        (name 'rde)
        (url "https://git.sr.ht/~abcdw/rde")
        (branch "master")
        (commit
          "f88d8e26b1dca922a457aad9c2c14b4afd589340")
        (introduction
          (make-channel-introduction
            "257cebd587b66e4d865b3537a9a88cccd7107c95"
            (openpgp-fingerprint
              "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "29d63cbac7b1e652932595adb583fcffe59bfaee")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'dinos)
        (url "https://gitlab.ody5.de/ody55eus/dinos.git")
        (branch "main")
        (commit
          "157a43deba12a81f0094ba7830d39eb5f0fcf8f1")
        (introduction
          (make-channel-introduction
            "e802e65a76e49435ded5ef909120b2b84f2e262b"
            (openpgp-fingerprint
              "885B 941B 8221 3321 6D96  0E4C DE2A D6CF 2474 B880"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "fe554337de7fa739165958e655fa6fb9dc73cf06")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
