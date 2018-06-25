import Control.Concurrent.STM

type Semaphore = TVar Bool

p :: Semaphore -> IO ()
p sem = do
	atomically ( do
		state <- readTVar sem
		if state then do
			writeTVar sem False
		else
			retry
		)

s :: Semaphore -> IO ()
s sem = do
	atomically ( do
		writeTVar sem True
		)

main :: IO ()
main = do
	sem <- atomically $ newTVar True
	p sem
	s sem
	p sem
	s sem
	s sem
	p sem
	putStrLn "End main"