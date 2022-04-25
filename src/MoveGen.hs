import Pieces
import Board

data RayDirections = RayDirections [Direction] Int

-- pawns capture differently than movement
data PieceMoveType = Same RayDirections | Different RayDirections RayDirections

data MoveType = Move [Ray] | Capture [Ray] | MoveCapture [Ray]

cardinals :: [Direction]
cardinals = [up, down, left, right]

diagonals :: [Direction]
diagonals = [up + left, up + right, down + left, down + right]