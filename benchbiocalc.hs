{-# LANGUAGE BangPatterns #-}

import BioCalc
import Data.Time.Clock
import qualified Data.ByteString.Char8 as BS

import Align

psba_pro :: BS.ByteString
psba_pro = BS.pack "VPSSNAIGLHFYPIWEAATLDEWLYNGGPYQLVIFHFLIGISAYMGRQWELSYRLGMRPWICVAYSAPVSAAFAVFLVYPFGQGSFSDGMPLGISGTFNFMFVFQAEHNILMHPFHMAGVAGMFGGALFSAMHGSLVTSSLIRETTGLDSQNYGYKFGQEEETYNIVAAHGYFGRLIFQYASFNNSRSLHFFLASWPVICVWLTSMGICTMAFNLNGFNFNQSVVDTSGKVVPTWGDVLNRANL"

psba_arab :: BS.ByteString
psba_arab = BS.pack "MTAILERRESESLWGRFCNWITSTENRLYIGWFGVLMIPTLLTATSVFIIAFIAAPPVDIDGIREPVSGSLLYGNNIISGAIIPTSAAIGLHFYPIWEAASVDEWLYNGGPYELIVLHFLLGVACYMGREWELSFRLGMRPWIAVAYSAPVAAATAVFLIYPIGQGSFSDGMPLGISGTFNFMIVFQAEHNILMHPFHMLGVAGVFGGSLFSAMHGSLVTSSLIRETTENESANEGYRFGQEEETYNIVAAHGYFGRLIFQYASFNNSRSLHFFLAAWPVVGIWFTALGISTMAFNLNGFNFNQSVVDSQGRVINTWADIINRANLGMEVMHERNAHNFPLDLAAVEAPSTNG"

main = do
    start <- getCurrentTime
    let !r = Align.align psba_arab psba_pro
    end <- getCurrentTime
    putStrLn $ "align took " ++ show (diffUTCTime end start)