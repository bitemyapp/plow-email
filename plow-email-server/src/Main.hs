import Plow.Email.Server


import Yesod.Core


main = startServer MailFoundation

startServer :: MailFoundation -> IO ()
startServer mf = warp 2633 mf
