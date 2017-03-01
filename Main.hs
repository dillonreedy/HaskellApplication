import Dijkstra
import Data.List
import System.IO
import Control.Monad
import Data.Matrix
import Data.Char

-- There are 2 representations that we need to make of our matrix
-- One will be the numbering of the elements (nameMatrix)
-- 1 2 3
-- 4 5 6
-- 7 8 9
--
-- The second will be the actual heights at those given locations (heightMatrix)
-- 15 16 17
-- 4 8 9
-- 2 10 12

-- This function calculates the edge cost between two points
getEdgeCost givenHeightMatrix startPt endPt = abs ((givenHeightMatrix ! startPt) - (givenHeightMatrix ! endPt))

-- This function determines if a given point is in bounds.
isInBounds givenPt maxX maxY = ((0 < (fst givenPt)) && ((fst givenPt) <= maxX)) && ((0 < (snd givenPt)) && ((snd givenPt) <= maxY))

-- This function just gives us numbers to add to get the adjacent points.
getAdjacentPts pt = [((fst pt) + i, (snd pt) + j) | i <- [-1..1], j <- [-1..1]]

-- This function will filter out adjacent points that are equal to the starting point or go out of bounds.
filterAdjacentPts startPt [] maxX maxY = []
filterAdjacentPts startPt (givenPt:givenPts) maxX maxY =
	if (((fst startPt) == (fst givenPt)) && ((snd startPt) == (snd givenPt))) then (filterAdjacentPts startPt givenPts maxX maxY)
	else if (isInBounds givenPt maxX maxY) then givenPt:(filterAdjacentPts startPt givenPts maxX maxY)
	else (filterAdjacentPts startPt givenPts maxX maxY)

-- We return back a list that is formatted as such:
-- (startPt, [adjacentPt1, adjacentPt2..])
getAdjacencyList [] maxX maxY = []
getAdjacencyList (givenPt:givenPts) maxX maxY = (givenPt, (filterAdjacentPts givenPt (getAdjacentPts givenPt) maxX maxY)):(getAdjacencyList givenPts maxX maxY)

-- This function will return back a list of tuples that are formatted as follows:
-- [(startPt, endPt1, edgeCost1), (startPt, endPt2, edgeCost2)..]
getEdgeCosts _ _ _ [] = []
getEdgeCosts givenHeightMatrix givenNameMatrix startPt (endPt:endPts) = ((givenNameMatrix ! startPt), (givenNameMatrix ! endPt), (getEdgeCost givenHeightMatrix startPt endPt)):(getEdgeCosts givenHeightMatrix givenNameMatrix startPt endPts)

getAllEdgeCosts' _ _ [] = []
getAllEdgeCosts' givenHeightMatrix givenNameMatrix (givenAdjacency:givenAdjacencyLis) =
 (getEdgeCosts givenHeightMatrix givenNameMatrix (fst givenAdjacency) (snd givenAdjacency)):(getAllEdgeCosts' givenHeightMatrix givenNameMatrix givenAdjacencyLis)

getAllEdgesAndCosts' givenHeightMatrix maxX maxY = getAllEdgeCosts' givenHeightMatrix (fromList maxX maxY [1..]) (getAdjacencyList [(i, j) | i <- [1..maxX], j <- [1..maxY]] maxX maxY)

getAllEdgesAndCosts givenHeightMatrix maxX maxY = concat (getAllEdgesAndCosts' givenHeightMatrix maxX maxY)

-- Functions to handle getting the elements from a 3-element tuple.
getFst (a,_,_) = a
getSnd (_,b,_) = b
getThrd (_,_,c) = c

-- Given a list of triple tuples, where it represents (startNode, endNode, edgeCost)
-- Write a list of strings such that it looks like the following:
-- ["startNode endNode edgeCost", "startNode endNode edgeCost"..]
getStrLis [] = []
getStrLis (givenEdgeAndCost:givenEdgesAndCosts) =
	("" ++ (show (getFst givenEdgeAndCost)) ++ " " ++ (show (getSnd givenEdgeAndCost)) ++ " " ++ (show (getThrd givenEdgeAndCost)) ++ ['\n']):(getStrLis givenEdgesAndCosts)

-- A function that converts a string to an integer.
readInt :: String -> Int
readInt = read


-- A function that given a matrix, returns back a vector that contains all the possible starting points
getStartingPts' numRows numCols = take numRows [1,(1+numCols)..]
getStartingPts givenMatrix = (getStartingPts' (nrows givenMatrix) (ncols givenMatrix))

-- A function that given a matrix, returns back all the possible ending points. Returns back as a list.
getEndingPts' numRows numCols = take numRows [numCols,(2*numCols)..]
getEndingPts givenMatrix = (getEndingPts' (nrows givenMatrix) (ncols givenMatrix))

-- Given the list of starting points, we get the Dijkstra scores of all the
getAllSolns [] _ = []
getAllSolns (startingPt:startingPts) g = (dijkstra g (show startingPt)):(getAllSolns startingPts g)

-- Given two float values, return the minimum of the two.
getFloatMin val1 val2 =
	if (val1 < val2) then val1
	else val2

-- Given a list find the minimum distance
-- Using the list at this website: https://en.wikipedia.org/wiki/List_of_elevation_extremes_by_country
-- We can determine the maximum elevation (in ft) happenning as of 2/26/2017
-- From there we can just return (maximum height) + 1, which happens to be 29030.0
getMinimumDistance'' _ [] = 29030.0
getMinimumDistance'' endingPt (curNode:curDijkstraSoln) =
	if ((show endingPt) == (fst curNode)) then (getFloatMin (fst (snd curNode)) (getMinimumDistance'' endingPt curDijkstraSoln))
	else (getMinimumDistance'' endingPt curDijkstraSoln)

-- Given a list of ending points and a given Dijkstra solution, return back the minimum cost to navigate to any of the ending locations
getMinimumDistance' [] _ = 29030.0
getMinimumDistance' (endingPt:endingPts) givenDijkstraSoln = (getFloatMin (getMinimumDistance'' endingPt givenDijkstraSoln) (getMinimumDistance' endingPts givenDijkstraSoln))

-- Given a list of ending points and a list of solutions on given starting points.
getMinimumDistance _ [] = 29030.0
getMinimumDistance endingPts (curDijkstraSoln:givenDijkstraSolns) = (getFloatMin (getMinimumDistance' endingPts curDijkstraSoln) (getMinimumDistance endingPts givenDijkstraSolns))

-- Get the start and end nodes that have the minimum distance
-- Given one solution, return back the node if its' distance is equal to the minimum.
getMinimumPathEndNodes [] _ _ _ = []
getMinimumPathEndNodes (givenNode:givenSol) unchangedSol givenMin givenEndNodes =
	if ((isEndNode (snd (snd givenNode)) givenEndNodes) && ((show givenMin) == (show (fst (snd givenNode))))) then (pathToNode unchangedSol (fst givenNode)):(getMinimumPathEndNodes givenSol unchangedSol givenMin givenEndNodes)
	else (getMinimumPathEndNodes givenSol unchangedSol givenMin givenEndNodes)

isEndNode _ [] = False
isEndNode givenNode (givenEndNode:givenEndNodes) =
	if (givenNode == (show givenEndNode)) then True
	else (isEndNode givenNode givenEndNodes)


main = do
	print "What is the file name?"
	fileName <- getLine
	contents <- readFile (fileName ++ ".txt")
	let inputLis = map readInt . words $ contents
	let rows = inputLis !! 0
	let cols = inputLis !! 1
	let inputTable = drop 2 inputLis
	let inputMatrix = (fromList rows cols inputTable)
	--print (([(intToDigit rows)] ++ "," ++ [(intToDigit cols)]))
	--print inputTable
	--print inputMatrix
	--print (getStrLis (getAllEdgesAndCosts inputMatrix rows cols))
	let outputStr = (concat (getStrLis (getAllEdgesAndCosts inputMatrix rows cols)))
	let g = fromText outputStr True
	--print (getAllSolns (getStartingPts inputMatrix) g)
	let allSolns = (getAllSolns (getStartingPts inputMatrix) g)
	print (zip (getStartingPts inputMatrix) allSolns)
	--print allSolns
	let minToEnd = (getMinimumDistance (getEndingPts inputMatrix) allSolns)
	print minToEnd
	--print (edgesFor g "1")
	--let soln = dijkstra g "1"
	--print soln
	--print (pathToNode soln "9")
