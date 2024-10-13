-- tests/TestCompiler.hs

module Main where

import HaskelCompiler  -- Використовується імпорт модуля компілятора

main :: IO ()
main = do
  -- Набір інструкцій для компіляції
  let instructions = [Load "eax" 0x1000, Add "eax" "ebx", Store "eax" 0x2000]
  -- Записати бінарний файл з машинним кодом
  compileAndWrite instructions "output.bin"
  putStrLn "Compilation finished. Check output.bin for the generated binary code."
