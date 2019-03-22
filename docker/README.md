# RCB-deploy
### Instructions

Run `./build.sh` every time you make any changes or commits to the RCB code.  This would pull latest commit from the project github and build new docker image. 

R versions could be fontrolled withing the `Dockerfile` with `From` statement. 
Available versions of R listed in here - https://hub.docker.com/_/r-base/?tab=tags



RUn `./rcb.sh XXX` as before. 

Note: You might need to enter github credentials during project checkout. 

TIP: Save git creds
`git config credential.helper store`
