# Automated build based on architecture

user=`whoami`
if [ ! -d /home/$user/store ]; then
  echo "Creating /home/$user/store directory"
  mkdir /home/$user/store
  chmod 751 cpls
fi
if [ ! -f /home/$user/store/config.R ]; then
  echo "Copying default config.R"
  docker run -u user:user -v /home/$user/store:/home/user/cpls/store cpls cp /home/user/cpls/data/config.R /home/user/cpls/store
  echo "Copying default user_name.acc"
  docker run -u user:user -v /home/$user/store:/home/user/cpls/store cpls cp /home/user/cpls/data/user_name.acc /home/user/cpls/store 
fi

arch | grep arm > /dev/null
if [ $? -eq 0 ]; then
  cd arm
else
  cd linux
fi

docker build -t cpls .  
