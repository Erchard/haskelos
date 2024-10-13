-- HaskelCompiler.hs

module HaskelCompiler where

import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 ()
import Data.Bits (shiftR)

-- Основний тип даних для проміжного представлення (IR)
data IR = Load Reg MemAddr
         | Store Reg MemAddr
         | Add Reg Reg
         | Jump Label
         | Compare Reg Reg
         deriving (Show, Eq)

-- Типи для регістрів, адрес пам'яті, та міток типів даних
type Reg = String
type MemAddr = Int
type Label = String

-- Функція компіляції IR в машинний код
compileIR :: IR -> [Word8]
compileIR (Load reg addr) = [0xA1] ++ encodeReg reg ++ encodeAddr addr
compileIR (Store reg addr) = [0xA3] ++ encodeReg reg ++ encodeAddr addr
compileIR (Add reg1 reg2) = [0x01] ++ encodeReg reg1 ++ encodeReg reg2
compileIR (Jump lbl) = [0xE9] ++ encodeLabel lbl
compileIR (Compare reg1 reg2) = [0x39] ++ encodeReg reg1 ++ encodeReg reg2

-- Допоміжні функції для кодування регістрів, адрес, та міток
encodeReg :: Reg -> [Word8]
encodeReg reg = case reg of
  "eax" -> [0x00]
  "ebx" -> [0x03]
  _      -> error "Unsupported register"

encodeAddr :: MemAddr -> [Word8]
encodeAddr addr = [fromIntegral (addr `shiftR` 24), fromIntegral (addr `shiftR` 16), fromIntegral (addr `shiftR` 8), fromIntegral addr]

encodeLabel :: Label -> [Word8]
encodeLabel _ = [0x00, 0x00, 0x00, 0x00]  -- Тимчасова реалізація для тестування

-- Функція для генерації бінарного файлу з машинним кодом
writeBinary :: [Word8] -> FilePath -> IO ()
writeBinary bytes path = B.writeFile path (B.pack bytes)

-- Приклад компіляції набору інструкцій і запису в файл
compileAndWrite :: [IR] -> FilePath -> IO ()
compileAndWrite irs path = writeBinary (concatMap compileIR irs) path