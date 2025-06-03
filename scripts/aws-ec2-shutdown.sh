#! /bin/bash
export LC_ALL="C"
aws ec2 stop-instances --instance-ids i-01f4650cf64ee0e2d --region us-east-2 --profile default
