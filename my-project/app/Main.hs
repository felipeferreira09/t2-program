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

execNOP :: Estado -> Estado
execNOP estado = estado

execLOD :: Int -> Estado -> Estado
execLOD end estado = estado { acc = valor, eqz = valor == 0 }
    where valor = readMem (memoria estado) end

execSTO :: Int -> Estado -> Estado
execSTO end estado = estado { memoria = writeMem (memoria estado) end (acc estado) }

execJMP :: Int -> Estado -> Estado
execJMP end estado = estado { pc = end }

execJMZ :: Int -> Estado -> Estado
execJMZ end estado
    | eqz estado = estado { pc = end }
    | otherwise = estado

execCPE :: Int -> Estado -> Estado
execCPE end estado
    | valor == acc estado = estado { acc = 0, eqz = True }
    | otherwise = estado { acc = 1, eqz = False }
    where valor = readMem (memoria estado) end

execADD :: Int -> Estado -> Estado
execADD end estado = estado { acc = resultado, eqz = resultado == 0 }
    where resultado = (acc estado + readMem (memoria estado) end) `mod` 256

execSUB :: Int -> Estado -> Estado
execSUB end estado = estado { acc = resultado, eqz = resultado == 0 }
    where resultado = (acc estado - readMem (memoria estado) end) `mod` 256

execHLT :: Estado -> Estado
execHLT estado = estado

executarInstrucao :: Estado -> Estado
executarInstrucao estado =
    let pcAtual = pc estado
        opcode = readMem (memoria estado) pcAtual
        endereco = readMem (memoria estado) (pcAtual + 1)
        novoEstado = case opcode of
            2 -> execLOD endereco estado
            4 -> execSTO endereco estado
            6 -> execJMP endereco estado
            8 -> execJMZ endereco estado
            10 -> execCPE endereco estado
            14 -> execADD endereco estado
            16 -> execSUB endereco estado
            18 -> execNOP estado
            20 -> execHLT estado
            _ -> estado
    in novoEstado { pc = pcAtual + 2 }

executar :: Estado -> Estado
executar estado =
    let novoEstado = executarInstrucao estado
    in if pc novoEstado == pc estado then estado else executar novoEstado

prog1 :: Memoria
prog1 = [(0, 2), (1, 240), -- LOD 240
         (2, 14), (3, 241), -- ADD 241
         (4, 16), (5, 245), -- SUB 245
         (6, 4), (7, 251),  -- STO 251
         (8, 20), (9, 18),  -- HLT NOP
         (240, 5),          -- Valor de A
         (241, 3),          -- Valor de B
         (245, 2),          -- Constante 2
         (251, 0)]          -- Resultado inicializado em 0

prog2 :: Memoria
prog2 = [(0, 2), (1, 240),  -- LOD 240
         (2, 4), (3, 250),  -- STO 250 (Armazena A em 250)
         (4, 2), (5, 251),  -- LOD 251 (Carrega 0 inicial em Resp)
         (6, 18), (7, 18),  -- NOP NOP (Espaço para loop)
         (8, 14), (9, 241), -- ADD 241 (Soma B)
         (10, 4), (11, 251), -- STO 251 (Armazena em Resp)
         (12, 16), (13, 250), -- SUB 250 (Decrementa A)
         (14, 8), (15, 18),  -- JMZ 18 (Salta se A == 0)
         (16, 6), (17, 8),   -- JMP 8 (Loop)
         (18, 20), (19, 18), -- HLT NOP
         (240, 3),           -- Valor de A
         (241, 2),           -- Valor de B
         (250, 0),           -- Temporário para A
         (251, 0)]           -- Resultado inicializado em 0

prog3 :: Memoria
prog3 = [(0, 2), (1, 245),   -- LOD 245 (Carrega 0 em A)
         (2, 4), (3, 240),   -- STO 240 (Armazena A)
         (4, 2), (5, 246),   -- LOD 246 (Carrega 1 em Resp)
         (6, 4), (7, 251),   -- STO 251 (Armazena Resp)
         (8, 2), (9, 240),   -- LOD 240 (Carrega A)
         (10, 14), (11, 247), -- ADD 247 (Incrementa A)
         (12, 4), (13, 240), -- STO 240 (Armazena A)
         (14, 2), (15, 251), -- LOD 251 (Carrega Resp)
         (16, 14), (17, 248), -- ADD 248 (Soma 2)
         (18, 4), (19, 251), -- STO 251 (Armazena Resp)
         (20, 2), (21, 240), -- LOD 240 (Carrega A)
         (22, 16), (23, 249), -- SUB 249 (Verifica se A < 5)
         (24, 8), (25, 8),   -- JMZ 8 (Salta se A == 5)
         (26, 6), (27, 8),   -- JMP 8 (Loop)
         (28, 20), (29, 18), -- HLT NOP
         (240, 0),           -- Valor inicial de A
         (245, 0),           -- Constante 0
         (246, 1),           -- Constante 1
         (247, 1),           -- Constante 1 para incrementar A
         (248, 2),           -- Constante 2 para incrementar Resp
         (249, 5),           -- Constante 5 para a condição do loop
         (251, 0)]           -- Valor inicial de Resp

main :: IO ()
main = do
    -- Executando Programa 1
    putStrLn "Executando Programa 1: Resp = A + B - 2"
    let resultado1 = executar (Estado { memoria = prog1, acc = 0, eqz = False, pc = 0 })
    print resultado1
    
    -- Executando Programa 2
    putStrLn "\nExecutando Programa 2: Resp = A * B"
    let resultado2 = executar (Estado { memoria = prog2, acc = 0, eqz = False, pc = 0 })
    print resultado2
    
    -- Executando Programa 3
    putStrLn "\nExecutando Programa 3: A = 0; Resp = 1; while(A < 5) { A = A + 1; Resp = Resp + 2; }"
    let resultado3 = executar (Estado { memoria = prog3, acc = 0, eqz = False, pc = 0 })
    print resultado3
