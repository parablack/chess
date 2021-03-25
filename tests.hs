import Chess
import ParseFEN

-- https://www.chessprogramming.org/Perft_Results

assertPerft state depth num
    | result == num = putStrLn $ "Test Passed! " ++ show num
    | result /= num = putStrLn $ "Test Failed! " ++ show num ++ " /= " ++ show result
    where result = perft depth state

testPosition string tests =
    let
        state = parseFen string
        process [] = return ()
        process ((i, num) : tests) = do
            assertPerft state i num
            process tests
    in do
        putStrLn $ "State " ++ string
        process tests

testInitialPosition = testPosition
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    [(0, 1), (1, 20), (2, 400), (3, 8902), (4, 197281), (5, 4865609)]
    -- (5, 119060324) (6, 3195901860)

testPosition2 = testPosition
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -"
    [(1, 48), (2, 2039), (3, 97862), (4, 4085603)]
    -- (5, 193690690) (6, 8031647685)

testPosition3 = testPosition
    "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -"
    [(1, 14), (2, 191), (3, 2812), (4, 43238), (5, 674624)]
    -- (6, 11030083) (7, 178633661) (8, 3009794393)

testPosition4 = testPosition
    "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
    [(1, 6), (2, 264), (3, 9467), (4, 422333)]
    -- (5, 15833292) (6, 706045033)

testPosition5 = testPosition
    "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
    [(1, 44), (2, 1486), (3, 62379), (4, 2103487)]
    -- (5, 89941194)

testPosition6 = testPosition
    "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
    [(0, 1), (1, 46), (2, 2079), (3, 89890), (4, 3894594)]
    -- (5, 164075551), (6, 6923051137) (7, 287188994746)

main = do
    testInitialPosition
    testPosition2
    testPosition3
    testPosition4
    testPosition5
    testPosition6
