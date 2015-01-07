module BoardTest (tests) where

import Distribution.TestSuite
import Board

tests :: IO [Test]
tests = return 	[ Test setChipTest
				, Test freeRowTest1
				, Test freeRowTest2
				, Test freeRowTest3
				, Test notDeadTest1
				, Test notDeadTest2
				]
    where
        setChipTest = TestInstance
            { name = "setChipTest"
            , run = return $ Finished $ do
                                            if (setChip 2 4 2 sampleBoard == sampleBoard2) then Pass else Fail "boards not equal"
            , tags = []
            , options = []
            , setOption = \_ _ -> Right setChipTest
            }

        freeRowTest1 = TestInstance
            { name = "freeRowTest1"
            , run = return $ Finished $ do
                                            case freeRow 2 sampleBoard of
                                            	Nothing -> Fail "Nothing instead 4"
                                            	Just 4	-> Pass
                                            	Just x	-> Fail (show x ++ " instead 4")
            , tags = []
            , options = []
            , setOption = \_ _ -> Right freeRowTest1
            }
        freeRowTest2 = TestInstance
            { name = "freeRowTest2"
            , run = return $ Finished $ do
                                            case freeRow 0 sampleBoard2 of
                                            	Nothing -> Fail "Nothing instead 5"
                                            	Just 5	-> Pass
                                            	Just x	-> Fail (show x ++ " instead 5")
            , tags = []
            , options = []
            , setOption = \_ _ -> Right freeRowTest2
            }
        freeRowTest3 = TestInstance
            { name = "freeRowTest3"
            , run = return $ Finished $ do
                                            case freeRow 4 $ replicate boardHeight $ replicate boardWidth 1 of
                                            	Nothing -> Pass
                                            	Just x	-> Fail (show x ++ " instead Nothing")
            , tags = []
            , options = []
            , setOption = \_ _ -> Right freeRowTest3
            }

        notDeadTest1 = TestInstance
            { name = "notDeadTest1"
            , run = return $ Finished $ do
                                            case notDead sampleBoard of
                                            	True  -> Pass
                                            	False -> Fail "Board is not dead"
            , tags = []
            , options = []
            , setOption = \_ _ -> Right notDeadTest1
            }
        notDeadTest2 = TestInstance
            { name = "notDeadTest2"
            , run = return $ Finished $ do
                                            case notDead $ replicate boardHeight $ replicate boardWidth 1 of
                                            	True  -> Fail "Board is dead"
                                            	False -> Pass
            , tags = []
            , options = []
            , setOption = \_ _ -> Right notDeadTest2
            }
 

sampleBoard :: Board
sampleBoard = [replicate boardWidth 0,
			   replicate boardWidth 0,
			   replicate boardWidth 0,
			   replicate boardWidth 0,
			   replicate boardWidth 0,
			   [0, 1, 2, 1, 0, 1, 0]]

sampleBoard2 :: Board
sampleBoard2 = [replicate boardWidth 0,
			   replicate boardWidth 0,
			   replicate boardWidth 0,
			   replicate boardWidth 0,
			   [0, 0, 2, 0, 0, 0, 0],
			   [0, 1, 2, 1, 0, 1, 0]]

