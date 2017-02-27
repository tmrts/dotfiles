#!/bin/sh

URL='http://www.accuweather.com/en/tr/ankara/10178/weather-forecast/178087'

curl -q "$URL" | awk -F\' '/acm_RecentLocationsCarousel\.push/{print $2": "$14", "$12"°" }'| head -1
