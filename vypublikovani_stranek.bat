git stash
git checkout devel
git push

stack exec site clean
stack exec site build

git fetch --all
git checkout -b master --track origin/master

robocopy _site .\

REM git add -A
REM git commit -m 'Publish'

REM git push origin master

REM git checkout devel
REM git branch -D master
REM git stash pop