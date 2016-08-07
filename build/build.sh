# Automated build based on architecture

dir=`pwd`
base=`basename $dir`
if [ base != 'build' ]; then
  echo "Err: Must be in build directory"
  exit
fi

if [ ! -d $HOME/store ]; then
  echo "Creating $HOME/store directory"
  mkdir $HOME/store
  chmod 751 $HOME/store
fi
if [ ! -f $HOME/store/config.R ]; then
  echo "Copying default config.R"
  docker run -u user:user -v $HOME/store:/home/user/cpls/store cpls cp /home/user/cpls/data/config.R /home/user/cpls/store
  echo "Copying default user_name.acc"
  docker run -u user:user -v $HOME/store:/home/user/cpls/store cpls cp /home/user/cpls/data/user_name.acc /home/user/cpls/store 
fi

arch | grep arm > /dev/null
if [ $? -eq 0 ]; then
  file='./build/arm/Dockerfile'
else
  file='./build/linux/Dockerfile'
fi

cd ..
docker build -t cpls -f $file .

