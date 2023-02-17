(use-modules (guix channels))

(list (channel
        (name 'rde)
        (url "https://git.sr.ht/~abcdw/rde")
        (branch "master")
        (commit
          "ce138d8cb8c02154a86b7ffb76ff932ca9cb802f")
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
          "63145a63fb40f92a8162eb2c1853e8b953f2ee79")
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
          "3910fa01d1d9b3d837f7989906d6f1a7aeca7b70")
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
          "246a3d90eac82966b691bdca4660ab9c5d802631")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
