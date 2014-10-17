import Plow.Email.Server


import Yesod


main = startServer MailFoundation

startServer :: MailFoundation -> IO ()
startServer mf = warp 2633 mf
