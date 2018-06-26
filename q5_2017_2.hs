import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent

atomicPrint :: String -> MVar () -> IO ()
atomicPrint str printLock = do
	takeMVar printLock
	putStrLn str
	putMVar printLock ()

supplierWorker :: TVar Int -> TVar Bool -> MVar () -> IO ()
supplierWorker item working printLock = do
	(atomically $ readTVar working) >>= (\state -> do
			if state then do
				atomically $ writeTVar item 30
				atomicPrint "Putting 30" printLock
				supplierWorker item working printLock
			else
				return ()
		)

preparerWorker :: TVar Int -> TVar Int -> TVar Int -> TVar Int -> MVar () -> TVar Bool -> MVar () -> IO ()
preparerWorker sands breads meats tomatoes knife working printLock = do
	withMVar knife (\_ -> do
		atomically ( do
			numBreads <- readTVar breads
			numMeats <- readTVar meats
			numTomatoes <- readTVar tomatoes
			if numBreads == 0 || numMeats == 0 || numTomatoes == 0 then
				retry
			else do
				writeTVar breads (numBreads-1)
				writeTVar meats (numMeats-1)
				writeTVar tomatoes (numTomatoes-1)
				(readTVar sands) >>= (\numSands -> writeTVar sands (numSands+1))
			)
		)
	atomicPrint "One sandwich produced" printLock
	(atomically $ readTVar working) >>= (\state -> do
		if state then do
			preparerWorker sands breads meats tomatoes knife working printLock
		else
			return ()
		)

main :: IO ()
main = do
	sandwiches <- atomically $ newTVar 0
	breads <- atomically $ newTVar 30
	meats <- atomically $ newTVar 30
	tomatoes <- atomically $ newTVar 30
	working <- atomically $ newTVar True
	knife <- newMVar ()
	printLock <- newMVar ()

	forkIO $ supplierWorker breads working printLock
	forkIO $ supplierWorker meats working printLock
	forkIO $ supplierWorker tomatoes working printLock
	forkIO $ preparerWorker sandwiches breads meats tomatoes knife working printLock
	forkIO $ preparerWorker sandwiches breads meats tomatoes knife working printLock

	threadDelay $ (10^6)
	atomically $ writeTVar working False
	(atomically $ readTVar sandwiches) >>= (\qt -> atomicPrint (show qt) printLock)
	putStrLn "Main end"