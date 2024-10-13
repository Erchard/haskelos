module Main where

import DiskImageCreator

main :: IO ()
main = do
    putStrLn "Creating disk image simple_os.img"
    createDiskImage "simple_os.img"
    putStrLn "Disk image successfully created."
