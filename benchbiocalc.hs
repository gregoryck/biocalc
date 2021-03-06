
import qualified Data.ByteString.Char8 as BS
import Criterion.Main

import AlignPar

psba_pro :: BS.ByteString
psba_pro = BS.pack "VPSSNAIGLHFYPIWEAATLDEWLYNGGPYQLVIFHFLIGISAYMGRQWELSYRLGMRPWICVAYSAPVSAAFAVFLVYPFGQGSFSDGMPLGISGTFNFMFVFQAEHNILMHPFHMAGVAGMFGGALFSAMHGSLVTSSLIRETTGLDSQNYGYKFGQEEETYNIVAAHGYFGRLIFQYASFNNSRSLHFFLASWPVICVWLTSMGICTMAFNLNGFNFNQSVVDTSGKVVPTWGDVLNRANL"

psba_arab :: BS.ByteString
psba_arab = BS.pack "MTAILERRESESLWGRFCNWITSTENRLYIGWFGVLMIPTLLTATSVFIIAFIAAPPVDIDGIREPVSGSLLYGNNIISGAIIPTSAAIGLHFYPIWEAASVDEWLYNGGPYELIVLHFLLGVACYMGREWELSFRLGMRPWIAVAYSAPVAAATAVFLIYPIGQGSFSDGMPLGISGTFNFMIVFQAEHNILMHPFHMLGVAGVFGGSLFSAMHGSLVTSSLIRETTENESANEGYRFGQEEETYNIVAAHGYFGRLIFQYASFNNSRSLHFFLAAWPVVGIWFTALGISTMAFNLNGFNFNQSVVDSQGRVINTWADIINRANLGMEVMHERNAHNFPLDLAAVEAPSTNG"

main :: IO ()
main = defaultMain [
        bgroup "fib" [ bench "align Pro and Arabidopsis psbA" $ whnf print $ align psba_arab psba_pro
                     ]
                    ]

-- main = do
--   benchmark 
--     let !r = Align.align psba_arab psba_pro
--     end <- getCurrentTime
--     putStrLn $ "align took " ++ show (diffUTCTime end start)
