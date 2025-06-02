#! /bin/bash
export LC_ALL="C"
aws ec2 stop-instances --instance-ids i-0b56b2d2d09e45b7d --region us-east-2 --profile default
