#! /bin/bash
export LC_ALL="C"
aws ec2 stop-instances --instance-ids i-014db74d5072c5399 --region us-east-2 --profile default
