module DiskImageCreator where

import System.IO
import Data.ByteString (hPut, ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Data.Binary.Put (runPut, putWord8, putByteString, putWord16le, putWord32le)

-- Конфігурація ОС
osName :: String
osName = "HaskelOS"

osVersion :: String
osVersion = "0.0.1"

sectorSize :: Int
sectorSize = 512

totalSectors :: Int
totalSectors = 65536  -- Приблизно 32 MB

-- Функція для створення простого завантажувального сектора, який лише виводить текст
bootSector :: ByteString
bootSector = B.concat [
    BL.toStrict $ runPut $ do
        putWord8 0xeb  -- Зміщення на початок завантажувального коду
        putWord8 0x3e  -- Зміщення для JMP
        putWord8 0x90,  -- NOP (no operation)
    B.pack (map (fromIntegral . fromEnum) "HASKEL_OS"),  -- Ідентифікаційний підпис
    B.replicate 3 0,  -- Порожні байти
    BL.toStrict $ runPut $ do
        putWord8 (fromIntegral sectorSize)
        putWord8 1,  -- Кількість секторів на кластер
    B.replicate 25 0,  -- Інші метадані (порожні)
    B.pack (map (fromIntegral . fromEnum) (osName ++ " v" ++ osVersion ++ "\0")),  -- Назва ОС і версія
    BL.toStrict $ runPut $ do
        putWord8 0xcd  -- Зупинка системи
        putWord8 0x19
        putWord8 0xb4
        putWord8 0x00,
    B.replicate (sectorSize - 90 - 66) 0,  -- Заповнення нулями до частини MBR
    -- Додавання базової таблиці розділів MBR
    BL.toStrict $ runPut $ do
        putByteString (B.replicate 446 0)  -- Завантажувальний код MBR (порожній)
        putWord8 0x80  -- Активний розділ
        putWord8 0x01  -- CHS початку розділу (головка)
        putWord8 0x01  -- CHS початку розділу (сектор)
        putWord8 0x00  -- CHS початку розділу (циліндр)
        putWord8 0x0B  -- Тип розділу (FAT32)
        putWord8 0xFF  -- CHS кінця розділу (головка)
        putWord8 0xFF  -- CHS кінця розділу (сектор)
        putWord8 0xFF  -- CHS кінця розділу (циліндр)
        putWord32le 1  -- Початок розділу (LBA)
        putWord32le (fromIntegral (totalSectors - 1))  -- Кількість секторів у розділі
        putByteString (B.replicate 48 0)  -- Інші записи таблиці розділів (порожні)
        putWord16le 0xAA55  -- Завершувальний підпис MBR
    ]

-- Функція створення образу диску
createDiskImage :: FilePath -> IO ()
createDiskImage filePath = withBinaryFile filePath WriteMode $ \h -> do
    -- Запис завантажувального сектора
    hPut h bootSector
    -- Заповнення решти образу диску нулями
    hPut h (B.replicate (sectorSize * (totalSectors - 1)) 0)