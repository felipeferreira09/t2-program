module Main where

type Memoria = [(Int, Int)]

readMem :: Memoria -> Int -> Int
readMem [] _ = 0
readMem ((addr, val):ms) e
    | addr == e = val
    | otherwise = readMem ms e

writeMem :: Memoria -> Int -> Int -> Memoria
writeMem [] addr val = [(addr, val)]
writeMem ((a, v):ms) addr val
    | a == addr = (a, val) : ms
    | otherwise = (a, v) : writeMem ms addr val

data Estado = Estado {
    memoria :: Memoria,
    acc :: Int,
    eqz :: Bool,
    pc :: Int
} deriving (Show)

execNOP, execHLT :: Estado -> Estado
execNOP estado = estado
execHLT estado = estado { pc = -1 } -- Marca PC como -1 para indicar parada

execLOD, execSTO, execJMP, execJMZ, execCPE, execADD, execSUB :: Int -> Estado -> Estado

execLOD end estado = estado { acc = valor, eqz = valor == 0 }
    where valor = readMem (memoria estado) end

execSTO end estado = estado { memoria = writeMem (memoria estado) end (acc estado) }

execJMP end estado = estado { pc = end }

execJMZ end estado
    | eqz estado = estado { pc = end }
    | otherwise = estado

execCPE end estado
    | valor == acc estado = estado { acc = 0, eqz = True }
    | otherwise = estado { acc = 1, eqz = False }
    where valor = readMem (memoria estado) end

execADD end estado = estado { acc = resultado, eqz = resultado == 0 }
    where resultado = (acc estado + readMem (memoria estado) end) `mod` 256

execSUB end estado = estado { acc = resultado, eqz = resultado == 0 }
    where resultado = (acc estado - readMem (memoria estado) end) `mod` 256

executarInstrucao :: Estado -> Estado
executarInstrucao estado =
    let pcAtual = pc estado
        opcode = readMem (memoria estado) pcAtual
        endereco = readMem (memoria estado) (pcAtual + 1)
        novoEstado = case opcode of
            2  -> execLOD endereco estado
            4  -> execSTO endereco estado
            6  -> execJMP endereco estado
            8  -> execJMZ endereco estado
            10 -> execCPE endereco estado
            14 -> execADD endereco estado
            16 -> execSUB endereco estado
            18 -> execNOP estado
            20 -> execHLT estado
            _  -> estado
    in if opcode /= 20 then novoEstado { pc = pcAtual + 2 } else novoEstado

executar :: Estado -> Estado
executar estado =
    let novoEstado = executarInstrucao estado
    in if pc novoEstado == -1 then novoEstado else executar novoEstado

-- Programas de Teste
prog1, prog2, prog3 :: Memoria

prog1 = [(0, 2), (1, 240), (2, 14), (3, 241), (4, 16), (5, 245), (6, 4), (7, 251), (8, 20), (9, 18), (240, 5), (241, 3), (245, 2), (251, 0)]

prog2 = [(0, 2), (1, 240), (2, 4), (3, 250), (4, 2), (5, 251), (6, 18), (7, 18), (8, 14), (9, 241), (10, 4), (11, 251), (12, 16), (13, 250), (14, 8), (15, 18), (16, 6), (17, 8), (18, 20), (19, 18), (240, 3), (241, 2), (250, 0), (251, 0)]

prog3 = [(0, 2), (1, 245), (2, 4), (3, 240), (4, 2), (5, 246), (6, 4), (7, 251), (8, 2), (9, 240), (10, 14), (11, 247), (12, 4), (13, 240), (14, 2), (15, 251), (16, 14), (17, 248), (18, 4), (19, 251), (20, 2), (21, 240), (22, 16), (23, 249), (24, 8), (25, 8), (26, 6), (27, 8), (28, 20), (29, 18), (240, 0), (245, 0), (246, 1), (247, 1), (248, 2), (249, 5), (251, 0)]

main :: IO ()
main = do
    putStrLn "Executando Programa 1: Resp = A + B - 2"
    let resultado1 = executar (Estado { memoria = prog1, acc = 0, eqz = False, pc = 0 })
    print resultado1

    putStrLn "\nExecutando Programa 2: Resp = A * B"
    let resultado2 = executar (Estado { memoria = prog2, acc = 0, eqz = False, pc = 0 })
    print resultado2

    putStrLn "\nExecutando Programa 3: A = 0; Resp = 1; while(A < 5) { A = A + 1; Resp = Resp + 2; }"
    let resultado3 = executar (Estado { memoria = prog3, acc = 0, eqz = False, pc = 0 })
    print resultado3
