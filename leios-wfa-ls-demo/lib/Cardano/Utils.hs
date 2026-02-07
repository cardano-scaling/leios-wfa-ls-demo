{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Utils where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf (printf)

import Cardano.Leios.Committee (OrderedSetOfParties (..), Party (..))

-- | Write the ordered stake distribution to a CSV for plotting
-- Also takes in the file path and the number of persistent voter
-- and the committee size for plotting.
writeStakeCSV :: FilePath -> Int -> Int -> OrderedSetOfParties -> IO ()
writeStakeCSV out persistentCount committeeSize OrderedSetOfParties {parties} = do
  let header =
        T.pack (printf "#persistent=%d\n" persistentCount)
          <> T.pack (printf "#committee=%d\n" committeeSize)
          <> T.pack "rank,stake\n"

      rows =
        [ T.pack (printf "%d,%.18f\n" (i :: Int) (fromRational (stake p) :: Double))
        | (i, p) <- zip [1 ..] parties
        ]

  T.writeFile out (header <> mconcat rows)
