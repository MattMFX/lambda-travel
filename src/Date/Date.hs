module Date.Date where

type Year = Int
data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show, Enum, Eq)
type Day = Int

data Date = Date Year Month Day


instance Show Date where
    show (Date y m d) = show d <> "/" <> show m <> "/" <> show y

instance Eq Date where
    (Date y1 m1 d1) == (Date y2 m2 d2) = y1 == y2 && m1 == m2 && d1 == d2

mkDate :: Int -> Int -> Int -> Date
mkDate d m y = Date year month day
    where
        year = if y > 0 then y else error "Invalid year"
        month = toEnum (m - 1) :: Month
        day = if d `elem` validDays month then d else error "Invalid day"

nextDate :: Date -> Date
nextDate (Date y m d) = Date year month day
    where
        lastDay = d == last (validDays m)
        lastMonth = m == December
        year = if lastDay && lastMonth then y + 1 else y
        month = if lastDay then nextMonth m else m
        day = if lastDay then 1 else d + 1

nextMonth :: Month -> Month
nextMonth m = if m == December then January else toEnum (fromEnum m + 1)

validDays :: Month -> [Int]
validDays month = case month of
    January -> [1..31]
    February -> [1..28]
    March -> [1..31]
    April -> [1..30]
    May -> [1..31]
    June -> [1..30]
    July -> [1..31]
    August -> [1..31]
    September -> [1..30]
    October -> [1..31]
    November -> [1..30]
    December -> [1..31]