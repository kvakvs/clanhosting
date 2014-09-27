#!/bin/sh

DATADIR=/var/lib/riak
sudo service riak stop
sudo rm -rf $DATADIR/bitcask.old
sudo mv $DATADIR/bitcask $DATADIR/riak
sudo service riak start
