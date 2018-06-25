import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar

-- Shared counter implementation using Transactional Memory and MVars

-- Thread for shared counter increment
counterWorker :: TVar Int -> MVar Int -> Int -> IO ()
counterWorker counter finished 0 = do
	takeMVar finished >>= (\v -> putMVar finished (v-1))
counterWorker counter finished increments = do
	atomically ( do
		(readTVar counter) >>= (\v -> writeTVar counter (v+1))
		)
	counterWorker counter finished (increments-1)


-- A tradicional join implementation
joinWorkers :: MVar Int -> IO ()
joinWorkers finished = do
	v <- takeMVar finished
	if v == 0 then do
		putStrLn "Workers stopped"
		putMVar finished v
		return ()
	else do
		let temp = show v
		putStrLn $ temp ++ " Workers working"
		putMVar finished v
		joinWorkers finished

main = do
	counter <- atomically $ newTVar 0
	finished <- newMVar 3
	forkIO $ counterWorker counter finished 1000000
	forkIO $ counterWorker counter finished 1500000
	forkIO $ counterWorker counter finished 3000000
	-- waits for every thread finish its job
	joinWorkers finished

	-- prints final counter's value
	(atomically $ readTVar counter) >>= (\v -> putStrLn $ show v)
	putStrLn "Main done"