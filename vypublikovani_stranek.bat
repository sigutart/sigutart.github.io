git stash
git checkout devel
git push

stack exec site clean
stack exec site build

git fetch --all
git checkout -b master --track origin/master

robocopy _site .\ /E

git add -A
git commit -m 'Publish'

git push origin master

git checkout devel
git branch -D master
git stash pop