curl -c /tmp/cookies "https://drive.google.com/uc?export=download&id=0B5Ak7kR0-3O3NEZVMVNnaWRkOWM" > /tmp/intermezzo.html
curl -L -b /tmp/cookies "https://drive.google.com$(cat /tmp/intermezzo.html | grep -Po 'uc-download-link" [^>]* href="\K[^"]*' | sed 's/\&amp;/\&/g')" > local.db
