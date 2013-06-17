import System.HComedi

main :: IO ()
main = withHandle "/dev/comedi0"
       (\p -> do
           putStrLn "Hello")
                                   