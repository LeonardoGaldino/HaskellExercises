import Control.Concurrent
import Control.Monad

producer :: MVar Int -> IO ()
producer productMVar = forever $ do
	takeMVar productMVar >>= (\v -> do putMVar productMVar (v+1))

mounter :: [MVar Int] -> MVar Int -> IO ()
mounter products box = forever $ do
	v1 <- takeMVar p1
	v2 <- takeMVar p2
	v3 <- takeMVar p3
	if v1 > 0 && v2 > 0 && v3 > 0 then do
		putMVar p1 (v1-1)
		putMVar p2 (v2-1)
		putMVar p3 (v3-1)
		takeMVar box >>= (\v -> do putMVar box (v+1))
	else do
		putMVar p1 v1
		putMVar p2 v2
		putMVar p3 v3
	where
		p1 = head products
		p2 = head $ tail products
		p3 = head $ tail $ tail products

main :: IO ()
main = do
	products <- sequence [newMVar 0, newMVar 0, newMVar 0]
	box <- newMVar 0
	forM products (\prod -> forkIO $ producer prod)
	forkIO $ mounter products box
	threadDelay 1000000
	v <- takeMVar box
	putStrLn $ show v
	putStrLn "Oi"
