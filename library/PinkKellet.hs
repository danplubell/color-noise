module PinkKellet (kellet) where
kellet :: [Double] -> [Double] -- ^ pinked noise
kellet w = kellet' w 0 0 0 0 0 0 0
    where kellet' []         _  _  _  _  _  _  _  = []
          kellet' (white:ws) b0 b1 b2 b3 b4 b5 b6 = pink : kellet' ws b0' b1' b2' b3' b4' b5' b6'
            where b0' = 0.99886 * b0 + white * 0.0555179;
                  b1' = 0.99332 * b1 + white * 0.0750759;
                  b2' = 0.96900 * b2 + white * 0.1538520;
                  b3' = 0.86650 * b3 + white * 0.3104856;
                  b4' = 0.55000 * b4 + white * 0.5329522;
                  b5' = -0.7616 * b5 - white * 0.0168980;
                  pink = b0 + b1 + b2 + b3 + b4 + b5 + b6 + white * 0.5362;
                  b6' = white * 0.115926
