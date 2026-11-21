#!/bin/bash
#
# prod-2-aws.sh — the Data UI prod-2 AWS creation sequence as a
# script (Appendix A of the EC2-instance plan). Run from the control
# host with AWS CLI v2, jq, and an ADMINISTRATOR profile exported
# (AWS_PROFILE); every lookup is by tag or name, so nothing depends
# on local state or on evo-x2 surviving.
#
# Verbs:
#   foundation                     A1–A4: VPC/IGW/subnets/SG/key/role/
#                                  ECR repositories/Elastic IP
#   bake                           A5: builder instance -> golden AMI
#                                  -> builder terminated (secret-free)
#   launch [--fresh-data|--adopt-data|--from-snapshot] [--az AZ]
#          [--scratch] [--ami ID]  A6: production (or scratch)
#                                  instance + data volume + post-image
#   publish-eip                    A7: associate the Elastic IP; add
#                                  the ~/.ssh/config alias
#   snapshots-policy               A8: DLM hourly snapshots of the
#                                  tagged data volume (retain 48)
#   lifecycle                      A9: clone the evo-x2/ S3 lifecycle
#                                  rules onto prod-2/ prefixes (merge,
#                                  never replace)
#   budget                         A10: $200 tag-filtered budget,
#                                  email at 150/200 (BUDGET_EMAIL)
#   replace [--adopt-data|--from-snapshot|--fresh-data] [--az AZ]
#                                  A11: fence every matching instance,
#                                  build the data volume per lane,
#                                  launch, post-image, rebirth (the
#                                  AWS half of the failure ladder)
#   ssh-allow                      revoke every port-22 CIDR on the SG
#                                  and authorize the caller's /32
#   add-app <name> <domain>        widen the role's A-record condition
#                                  + create the ECR repository
#   retire <instance-id>           un-terminate-protect + terminate
#                                  (Donnie's word only)
#
# Every returned scalar is checked for empty/None — the AMI id
# included. A tag lookup matching more than one resource is an error.
# Creating over an existing named resource is refused. A single-writer
# flock (the production checkout lock) excludes concurrent
# replace/launch operators; the describe-then-launch sequence keeps
# its residual TOCTOU only against non-cooperating callers, which is
# the documented boundary.

set -euo pipefail

die() { echo "prod-2-aws.sh: $*" >&2; exit 1; }

# --- A0 constants (environment-overridable) --------------------------------
export AWS_REGION="${AWS_REGION:-us-east-1}"
export AWS_DEFAULT_REGION="${AWS_DEFAULT_REGION:-$AWS_REGION}"
export AWS_PAGER=
: "${AWS_PROFILE:?export an administrator profile first (AWS_PROFILE=...)}"
ACCOUNT_ID=$(aws sts get-caller-identity --query Account --output text)
[ "$ACCOUNT_ID" = 232168665831 ] || die "wrong account (${ACCOUNT_ID}; wanted 232168665831)."

SITE=prod-2
NAME=data-ui-prod-2
PROJECT=data-ui-prod-2
ARCH="${ARCH:-arm64}"
INSTANCE_TYPE="${INSTANCE_TYPE:-t4g.xlarge}"
BUCKET=data-ui-backups-232168665831
ZONE_ID=Z0152779JVWMFEK6RAKU
KEY_NAME=data-ui-prod-2
KEY_PUB="${KEY_PUB:-$HOME/.ssh/evo-x2-id.pub}"
APPS="${APPS:-backlog scenebook todos}"
T="{Key=Site,Value=$SITE},{Key=Project,Value=$PROJECT}"
TAGS="{Key=Name,Value=$NAME},$T"
WORK=$(mktemp -d); chmod 700 "$WORK"
REPO="$WORK/data-ui"
SSH="ssh -i ${KEY_PUB%.pub} -o UserKnownHostsFile=$WORK/known_hosts -o StrictHostKeyChecking=accept-new"

[ -f "$KEY_PUB" ] || die "no public key at $KEY_PUB (set KEY_PUB=...)."

# The single-writer lock: the production checkout lock's directory, as
# the prune script uses it — replace/launch operators exclude each
# other.
LOCK_ROOT="${CHECKOUT_LOCK_ROOT:-/data/data-ui/repos}"
LOCK="$LOCK_ROOT/production-aws.lock"
mkdir -p "$LOCK_ROOT"
exec 8>"$LOCK"
flock -n 8 || die "another prod-2-aws operator holds ${LOCK}."

clone_repo() {
    [ -d "$REPO/.git" ] && return 0
    git clone --depth 1 https://github.com/macnod/data-ui.git "$REPO" \
        || die "cannot clone the public repo (network?)."
    echo "repo at $(git -C "$REPO" rev-parse HEAD)"
}

aws_scalar() { # aws_scalar LABEL ARGS... — dies on failure/None/empty
    local label="$1"; shift
    local out
    out=$(aws "$@") || die "${label}: aws call failed."
    [ -n "$out" ] && [ "$out" != "None" ] \
        || die "${label}: empty/None result."
    echo "$out"
}

fn_scalar() { # fn_scalar LABEL FN ARGS... — runs a script FUNCTION
    # (lane_data_volume, launch_instance, instance_ip) with the same
    # checked-scalar discipline; aws_scalar would try `aws <fn> ...`
    # and die on ParamValidation.
    local label="$1"; shift
    local out
    out=$("$@") || die "${label}: call failed."
    [ -n "$out" ] && [ "$out" != "None" ] \
        || die "${label}: empty/None result."
    echo "$out"
}

aws_tag_one() { # aws_tag_one LABEL ARGS... — dies on a multi match
    local label="$1"; shift
    local out n
    out=$(aws_scalar "$label" "$@")
    n=$(wc -l <<<"$out")
    [ "$n" -eq 1 ] || die "${label}: matched ${n} resources; exactly one expected — resolve by hand."
    echo "$out"
}

find_vpc()    { aws_tag_one "vpc lookup" ec2 describe-vpcs --filters "Name=tag:Name,Values=$NAME" --query 'Vpcs[0].VpcId' --output text 2>/dev/null || true; }
find_sg()     { local v; v=$(find_vpc); [ -n "$v" ] || return 0; aws_tag_one "sg lookup" ec2 describe-security-groups --filters "Name=vpc-id,Values=$v" "Name=group-name,Values=$NAME" --query 'SecurityGroups[0].GroupId' --output text 2>/dev/null || true; }
find_eip()    { aws_tag_one "eip lookup" ec2 describe-addresses --filters "Name=tag:Name,Values=$NAME" --query 'Addresses[0].AllocationId' --output text 2>/dev/null || true; }
find_eip_ip() { aws_tag_one "eip ip lookup" ec2 describe-addresses --filters "Name=tag:Name,Values=$NAME" --query 'Addresses[0].PublicIp' --output text 2>/dev/null || true; }

az_pair() {
    read -r AZ1 AZ2 _ < <(aws ec2 describe-instance-type-offerings \
        --location-type availability-zone \
        --filters "Name=instance-type,Values=$INSTANCE_TYPE" \
        --query 'sort(InstanceTypeOfferings[].Location)' --output text)
    [ -n "${AZ1:-}" ] && [ -n "${AZ2:-}" ] || die "cannot find two AZs offering ${INSTANCE_TYPE}."
}

app_record_names_json() { # the role policy's A-record condition list
    local names="" APP
    for APP in $APPS; do
        names+="\"${APP}.demo.data-ui.com\","
    done
    echo "${names%,}"
}

write_role_policy() {
    local RECORD_NAMES
    RECORD_NAMES=$(app_record_names_json)
    cat > "$WORK/role-policy.json" <<JSON
{
  "Version": "2012-10-17",
  "Statement": [
    {"Sid": "EcrToken", "Effect": "Allow",
     "Action": "ecr:GetAuthorizationToken", "Resource": "*"},
    {"Sid": "EcrPushPull", "Effect": "Allow",
     "Action": ["ecr:BatchCheckLayerAvailability", "ecr:BatchGetImage",
                "ecr:GetDownloadUrlForLayer", "ecr:InitiateLayerUpload",
                "ecr:UploadLayerPart", "ecr:CompleteLayerUpload",
                "ecr:PutImage", "ecr:DescribeImages"],
     "Resource": "arn:aws:ecr:us-east-1:$ACCOUNT_ID:repository/data-ui/*"},
    {"Sid": "SiteObjects", "Effect": "Allow",
     "Action": ["s3:PutObject", "s3:GetObject",
                "s3:AbortMultipartUpload", "s3:ListMultipartUploadParts"],
     "Resource": "arn:aws:s3:::$BUCKET/prod-2/*"},
    {"Sid": "ReadSourceSiteForMoves", "Effect": "Allow",
     "Action": "s3:GetObject",
     "Resource": "arn:aws:s3:::$BUCKET/evo-x2/*"},
    {"Sid": "ListSitePrefixes", "Effect": "Allow",
     "Action": "s3:ListBucket", "Resource": "arn:aws:s3:::$BUCKET",
     "Condition": {"StringLike": {"s3:prefix":
       ["prod-2", "prod-2/*", "evo-x2", "evo-x2/*"]}}},
    {"Sid": "Route53Discovery", "Effect": "Allow",
     "Action": ["route53:ListHostedZones", "route53:GetChange"],
     "Resource": "*"},
    {"Sid": "Route53ReadZone", "Effect": "Allow",
     "Action": "route53:ListResourceRecordSets",
     "Resource": "arn:aws:route53:::hostedzone/$ZONE_ID"},
    {"Sid": "WildcardDns01Only", "Effect": "Allow",
     "Action": "route53:ChangeResourceRecordSets",
     "Resource": "arn:aws:route53:::hostedzone/$ZONE_ID",
     "Condition": {"ForAllValues:StringEquals": {
       "route53:ChangeResourceRecordSetsNormalizedRecordNames":
         ["_acme-challenge.demo.data-ui.com"],
       "route53:ChangeResourceRecordSetsRecordTypes": ["TXT"],
       "route53:ChangeResourceRecordSetsActions": ["UPSERT", "DELETE"]}}},
    {"Sid": "AppAddressRecordsOnly", "Effect": "Allow",
     "Action": "route53:ChangeResourceRecordSets",
     "Resource": "arn:aws:route53:::hostedzone/$ZONE_ID",
     "Condition": {"ForAllValues:StringEquals": {
       "route53:ChangeResourceRecordSetsNormalizedRecordNames":
         [$RECORD_NAMES],
       "route53:ChangeResourceRecordSetsRecordTypes": ["A"],
       "route53:ChangeResourceRecordSetsActions": ["UPSERT", "DELETE"]}}},
    {"Sid": "OwnAddress", "Effect": "Allow",
     "Action": "ec2:DescribeAddresses", "Resource": "*"}
  ]
}
JSON
}

ecr_lifecycle_file() {
    cat > "$WORK/ecr-lifecycle.json" <<'JSON'
{"rules": [
  {"rulePriority": 1, "description": "Keep ten arm64 deploy images",
   "selection": {"tagStatus": "tagged",
     "tagPrefixList": ["deploy-arm64-"],
     "countType": "imageCountMoreThan", "countNumber": 10},
   "action": {"type": "expire"}},
  {"rulePriority": 2, "description": "Keep ten amd64 deploy images",
   "selection": {"tagStatus": "tagged",
     "tagPrefixList": ["deploy-amd64-"],
     "countType": "imageCountMoreThan", "countNumber": 10},
   "action": {"type": "expire"}},
  {"rulePriority": 3, "description": "Untagged residue after one day",
   "selection": {"tagStatus": "untagged",
     "countType": "sinceImagePushed", "countUnit": "days",
     "countNumber": 1},
   "action": {"type": "expire"}}]}
JSON
}

# ---------------------------------------------------------------------------
# foundation — A1..A4
# ---------------------------------------------------------------------------

cmd_foundation() {
    clone_repo
    az_pair
    local AMI_PARAM BASE_AMI ROOT_DEVICE VPC_ID IGW_ID RTB_ID SUBNET1 SUBNET2 SG_ID
    AMI_PARAM="/aws/service/canonical/ubuntu/server/26.04/stable/current/${ARCH}/hvm/ebs-gp3/ami-id"
    BASE_AMI=$(aws_scalar "base AMI" ssm get-parameter --name "$AMI_PARAM" \
                   --query Parameter.Value --output text)
    ROOT_DEVICE=$(aws_scalar "root device" ec2 describe-images --image-ids "$BASE_AMI" \
                      --query 'Images[0].RootDeviceName' --output text)

    refuse_existing "VPC" "$(find_vpc)"
    echo "== VPC, internet gateway, subnets (the account has no default VPC) =="
    VPC_ID=$(aws_scalar "create-vpc" ec2 create-vpc --cidr-block 10.72.0.0/24 \
                 --tag-specifications "ResourceType=vpc,Tags=[$TAGS]" \
                 --query Vpc.VpcId --output text)
    aws ec2 modify-vpc-attribute --vpc-id "$VPC_ID" \
        --enable-dns-hostnames '{"Value":true}' || die "modify-vpc-attribute failed."
    IGW_ID=$(aws_scalar "create-internet-gateway" ec2 create-internet-gateway \
                 --tag-specifications "ResourceType=internet-gateway,Tags=[$TAGS]" \
                 --query InternetGateway.InternetGatewayId --output text)
    aws ec2 attach-internet-gateway --internet-gateway-id "$IGW_ID" --vpc-id "$VPC_ID" \
        || die "attach-internet-gateway failed."
    RTB_ID=$(aws_scalar "main route table" ec2 describe-route-tables \
                 --filters "Name=vpc-id,Values=$VPC_ID" "Name=association.main,Values=true" \
                 --query 'RouteTables[0].RouteTableId' --output text)
    aws ec2 create-route --route-table-id "$RTB_ID" \
        --destination-cidr-block 0.0.0.0/0 --gateway-id "$IGW_ID" || die "create-route failed."
    SUBNET1=$(aws_scalar "subnet ${AZ1}" ec2 create-subnet --vpc-id "$VPC_ID" \
                  --cidr-block 10.72.0.0/26 --availability-zone "$AZ1" \
                  --tag-specifications "ResourceType=subnet,Tags=[{Key=Name,Value=$NAME-$AZ1},$T]" \
                  --query Subnet.SubnetId --output text)
    SUBNET2=$(aws_scalar "subnet ${AZ2}" ec2 create-subnet --vpc-id "$VPC_ID" \
                  --cidr-block 10.72.0.64/26 --availability-zone "$AZ2" \
                  --tag-specifications "ResourceType=subnet,Tags=[{Key=Name,Value=$NAME-$AZ2},$T]" \
                  --query Subnet.SubnetId --output text)
    local S
    for S in "$SUBNET1" "$SUBNET2"; do
        aws ec2 modify-subnet-attribute --subnet-id "$S" --map-public-ip-on-launch \
            || die "modify-subnet-attribute failed."
    done

    echo "== security group and key =="
    local ADMIN_CIDR
    ADMIN_CIDR="$(curl -fsS https://checkip.amazonaws.com)/32"
    SG_ID=$(aws_scalar "create-security-group" ec2 create-security-group \
                --group-name "$NAME" --description 'Data UI prod-2' --vpc-id "$VPC_ID" \
                --tag-specifications "ResourceType=security-group,Tags=[$TAGS]" \
                --query GroupId --output text)
    aws ec2 authorize-security-group-ingress --group-id "$SG_ID" --ip-permissions \
        "IpProtocol=tcp,FromPort=22,ToPort=22,IpRanges=[{CidrIp=$ADMIN_CIDR}]" \
        "IpProtocol=tcp,FromPort=80,ToPort=80,IpRanges=[{CidrIp=0.0.0.0/0}]" \
        "IpProtocol=tcp,FromPort=443,ToPort=443,IpRanges=[{CidrIp=0.0.0.0/0}]" \
        || die "authorize-security-group-ingress failed."
    if ! aws ec2 describe-key-pairs --key-names "$KEY_NAME" >/dev/null 2>&1; then
        aws ec2 import-key-pair --key-name "$KEY_NAME" \
            --public-key-material "fileb://$KEY_PUB" || die "import-key-pair failed."
    else
        echo "Key pair ${KEY_NAME} already imported."
    fi

    echo "== instance role (one role; no IAM users, no access keys on prod-2) =="
    cat > "$WORK/trust.json" <<'JSON'
{"Version": "2012-10-17", "Statement": [{"Effect": "Allow",
  "Principal": {"Service": "ec2.amazonaws.com"},
  "Action": "sts:AssumeRole"}]}
JSON
    write_role_policy
    if aws iam get-role --role-name "$NAME" >/dev/null 2>&1; then
        echo "Role ${NAME} exists; re-putting the policy (idempotent)."
    else
        aws iam create-role --role-name "$NAME" \
            --assume-role-policy-document "file://$WORK/trust.json" \
            --tags Key=Project,Value="$PROJECT" || die "create-role failed."
    fi
    aws iam put-role-policy --role-name "$NAME" --policy-name site \
        --policy-document "file://$WORK/role-policy.json" || die "put-role-policy failed."
    if ! aws iam get-instance-profile --instance-profile-name "$NAME" >/dev/null 2>&1; then
        aws iam create-instance-profile --instance-profile-name "$NAME" \
            || die "create-instance-profile failed."
    fi
    aws iam add-role-to-instance-profile --instance-profile-name "$NAME" \
        --role-name "$NAME" || die "add-role-to-instance-profile failed."
    aws iam wait instance-profile-exists --instance-profile-name "$NAME" || true
    sleep 15   # IAM propagation; retry run-instances once if it still fails

    echo "== ECR repositories + the Elastic IP =="
    ecr_lifecycle_file
    local APP EIP_ALLOC EIP
    for APP in $APPS; do
        if ! aws ecr describe-repositories --repository-names "data-ui/$APP" >/dev/null 2>&1; then
            aws ecr create-repository --repository-name "data-ui/$APP" \
                --image-tag-mutability MUTABLE \
                --image-scanning-configuration scanOnPush=true \
                --encryption-configuration encryptionType=AES256 \
                --tags Key=Project,Value="$PROJECT" || die "create-repository failed."
        else
            echo "Repository data-ui/${APP} exists."
        fi
        aws ecr put-lifecycle-policy --repository-name "data-ui/$APP" \
            --lifecycle-policy-text "file://$WORK/ecr-lifecycle.json" \
            || die "put-lifecycle-policy failed."
    done
    EIP_ALLOC=$(find_eip)
    if [ -z "$EIP_ALLOC" ]; then
        EIP_ALLOC=$(aws_scalar "allocate-address" ec2 allocate-address --domain vpc \
                        --tag-specifications "ResourceType=elastic-ip,Tags=[$TAGS]" \
                        --query AllocationId --output text)
    else
        echo "Elastic IP already allocated (${EIP_ALLOC})."
    fi
    EIP=$(aws ec2 describe-addresses --allocation-ids "$EIP_ALLOC" \
              --query 'Addresses[0].PublicIp' --output text)
    echo
    echo "foundation complete:"
    echo "  VPC $VPC_ID  subnets $SUBNET1 $SUBNET2  SG $SG_ID"
    echo "  Elastic IP ${EIP} (${EIP_ALLOC})"
    echo "next: bake"
}

refuse_existing() { # refuse_existing LABEL VALUE
    [ -z "$2" ] || [ "$2" = "None" ] || die "$1 already exists ($2); refusing to create over it."
}

# ---------------------------------------------------------------------------
# bake — A5
# ---------------------------------------------------------------------------

golden_ami() { # the newest validated golden AMI for this arch (or empty)
    aws ec2 describe-images --owners self \
        --filters "Name=tag:Role,Values=prod-2-golden" \
                  "Name=tag:Validated,Values=true" "Name=architecture,Values=$ARCH" \
                  "Name=state,Values=available" \
        --query 'reverse(sort_by(Images,&CreationDate))[0].ImageId' \
        --output text 2>/dev/null | grep -v '^None$' || true
}

cmd_bake() {
    clone_repo
    az_pair
    local SETUP_SHA
    SETUP_SHA=$(git -C "$REPO" rev-parse HEAD)
    local BASE_AMI ROOT_DEVICE
    BASE_AMI=$(aws_scalar "base AMI" ssm get-parameter \
                   --name "/aws/service/canonical/ubuntu/server/26.04/stable/current/${ARCH}/hvm/ebs-gp3/ami-id" \
                   --query Parameter.Value --output text)
    ROOT_DEVICE=$(aws_scalar "root device" ec2 describe-images --image-ids "$BASE_AMI" \
                      --query 'Images[0].RootDeviceName' --output text)
    local VPC_ID SG_ID SUBNET1
    VPC_ID=$(find_vpc); [ -n "$VPC_ID" ] || die "no VPC tagged $NAME; run foundation first."
    SG_ID=$(find_sg);   [ -n "$SG_ID" ] || die "no security group; run foundation first."
    SUBNET1=$(aws ec2 describe-subnets --filters "Name=vpc-id,Values=$VPC_ID" \
                  --query 'sort_by(Subnets,&CidrBlock)[0].SubnetId' --output text)
    [ -n "$SUBNET1" ] && [ "$SUBNET1" != "None" ] || die "no subnet found."

    echo "== builder (no instance profile, no data volume, no credentials) =="
    local ROOT_MAP BUILDER_ID BUILDER_IP
    ROOT_MAP=$(jq -nc --arg d "$ROOT_DEVICE" '[{DeviceName:$d,
      Ebs:{VolumeSize:50, VolumeType:"gp3", Encrypted:true,
           DeleteOnTermination:true}}]')
    BUILDER_ID=$(aws_scalar "run builder" ec2 run-instances --image-id "$BASE_AMI" \
                     --instance-type "$INSTANCE_TYPE" --key-name "$KEY_NAME" \
                     --security-group-ids "$SG_ID" --subnet-id "$SUBNET1" \
                     --block-device-mappings "$ROOT_MAP" \
                     --metadata-options \
                     'HttpTokens=required,HttpEndpoint=enabled,HttpPutResponseHopLimit=1' \
                     --tag-specifications \
                     "ResourceType=instance,Tags=[{Key=Name,Value=$NAME-builder},$T]" \
                     --query 'Instances[0].InstanceId' --output text)
    aws ec2 wait instance-status-ok --instance-ids "$BUILDER_ID" \
        || die "builder never reached status-ok."
    BUILDER_IP=$(aws_scalar "builder ip" ec2 describe-instances --instance-ids "$BUILDER_ID" \
                     --query 'Reservations[0].Instances[0].PublicIpAddress' --output text)
    echo "builder ${BUILDER_ID} at ${BUILDER_IP}; running ops/prod-2-host.sh base..."
    $SSH "ubuntu@$BUILDER_IP" 'sudo bash -s -- base' \
        < "$REPO/ops/prod-2-host.sh" || die "host base script failed on the builder."
    $SSH "ubuntu@$BUILDER_IP" 'sudo cloud-init clean --logs --machine-id' \
        || die "cloud-init clean failed."
    aws ec2 stop-instances --instance-ids "$BUILDER_ID" >/dev/null \
        || die "stop-instances failed."
    aws ec2 wait instance-stopped --instance-ids "$BUILDER_ID" \
        || die "builder never stopped."

    echo "== golden AMI =="
    local AMI_NAME GOLDEN_AMI LINES
    AMI_NAME="$NAME-$(date -u +%Y%m%dT%H%M%SZ)"
    GOLDEN_AMI=$(aws_scalar "create-image" ec2 create-image --instance-id "$BUILDER_ID" \
                     --name "$AMI_NAME" \
                     --description 'Data UI prod-2 secret-free base; no data volume' \
                     --tag-specifications \
                     "ResourceType=image,Tags=[{Key=Name,Value=$AMI_NAME},{Key=Role,Value=prod-2-golden},{Key=SetupSHA,Value=$SETUP_SHA},{Key=SourceAMI,Value=$BASE_AMI},$T]" \
                     "ResourceType=snapshot,Tags=[{Key=Name,Value=$AMI_NAME},{Key=Role,Value=prod-2-golden},$T]" \
                     --query ImageId --output text)
    # The waiter's default budget (40x15s = 10 min) is shorter than a
    # 50 GiB encrypted snapshot: the 2026-11-20 bake's AMI landed ~12
    # min in, after the waiter had already given up. 120x15s = 30 min.
    aws ec2 wait image-available --image-ids "$GOLDEN_AMI" \
        --max-attempts 120 \
        || die "AMI never became available."
    # Count only EBS mappings: the base AMI carries non-EBS ephemeral
    # entries (sdb, sdc — no Ebs object) that propagate into the
    # golden AMI and would trip a count of every mapping.
    LINES=$(aws ec2 describe-images --image-ids "$GOLDEN_AMI" --output text \
                --query 'Images[0].BlockDeviceMappings[?Ebs].[DeviceName,Ebs.VolumeSize]' | wc -l)
    [ "$LINES" -eq 1 ] \
        || die "wanted exactly one EBS block-device line (the 50 GiB root), got ${LINES}."
    aws ec2 terminate-instances --instance-ids "$BUILDER_ID" >/dev/null
    echo "golden AMI ${GOLDEN_AMI} (${AMI_NAME}, SetupSHA ${SETUP_SHA})."
    echo "Validated=true is added only after Phase C (first bake) or the scratch drill (re-bakes)."
}

# ---------------------------------------------------------------------------
# launch — A6 (function bodies reused by replace)
# ---------------------------------------------------------------------------

launch_instance() { # $1 = subnet id, $2 = Name tag; prints the id
    aws ec2 run-instances --image-id "$GOLDEN_AMI" \
        --instance-type "$INSTANCE_TYPE" --key-name "$KEY_NAME" \
        --security-group-ids "$SG_ID" --subnet-id "$1" \
        --iam-instance-profile "Name=$NAME" \
        --metadata-options \
        'HttpTokens=required,HttpEndpoint=enabled,HttpPutResponseHopLimit=1' \
        --tag-specifications \
        "ResourceType=instance,Tags=[{Key=Name,Value=$2},$T]" \
        "ResourceType=volume,Tags=[{Key=Name,Value=$2-root},$T]" \
        --query 'Instances[0].InstanceId' --output text
}

instance_ip() { # $1 = instance id
    aws ec2 describe-instances --instance-ids "$1" --output text \
        --query 'Reservations[0].Instances[0].PublicIpAddress'
}

new_data_volume() { # $1 = AZ, $2 = Name tag; prints the volume id
    aws ec2 create-volume --availability-zone "$1" --size 100 \
        --volume-type gp3 --encrypted --tag-specifications \
        "ResourceType=volume,Tags=[{Key=Name,Value=$2},$T]" \
        --query VolumeId --output text
}

attach_data() { # $1 = volume id, $2 = instance id
    aws ec2 wait volume-available --volume-ids "$1" \
        || die "volume ${1} never became available."
    aws ec2 attach-volume --volume-id "$1" --instance-id "$2" --device /dev/sdf \
        || die "attach-volume failed."
    aws ec2 wait volume-in-use --volume-ids "$1" \
        || die "volume ${1} never reached in-use."
}

resolve_common() { # sets VPC_ID/SG_ID/EIP_ALLOC/EIP
    VPC_ID=$(find_vpc);  [ -n "$VPC_ID" ]  || die "no VPC; run foundation first."
    SG_ID=$(find_sg);    [ -n "$SG_ID" ]   || die "no security group; run foundation first."
    EIP_ALLOC=$(find_eip); [ -n "$EIP_ALLOC" ] || die "no Elastic IP; run foundation first."
    EIP=$(find_eip_ip);  [ -n "$EIP" ]     || die "no Elastic IP address; run foundation first."
}

# The AMI is resolved only where it is used: launch and replace
# (publish-eip needs none — demanding one there would strand Phase B.3
# after an unvalidated first bake). An explicit id may be unvalidated:
# the first launch needs it (Validated comes only after Phase C), and
# the scratch drill must name the just-baked AMI or golden_ami() would
# silently return the older validated image instead of the one under
# test. replace stays validated-only — the failure ladder launches
# proven images.
resolve_golden_ami() { # $1 (optional) = explicit AMI id; sets GOLDEN_AMI
    if [ -n "${1:-}" ]; then
        GOLDEN_AMI=$(aws_scalar "explicit AMI ${1} (wanted: available ${ARCH})" \
                         ec2 describe-images --image-ids "$1" \
                         --filters "Name=state,Values=available" \
                                   "Name=architecture,Values=$ARCH" \
                         --query 'Images[0].ImageId' --output text)
    else
        GOLDEN_AMI=$(golden_ami)
        [ -n "$GOLDEN_AMI" ] || die "no validated golden AMI for ${ARCH}; bake and validate one first (the scratch drill earns Validated=true), or launch with --ami <id>."
    fi
}

lane_data_volume() { # $1 = lane (fresh|adopt|snapshot), $2 = volume Name tag, $3 = AZ
    local lane="$1" VNAME="$2" AZ="$3"
    local OLD_VOL SNAP
    OLD_VOL=$(aws ec2 describe-volumes --filters "Name=tag:Name,Values=$VNAME" \
                  --query 'Volumes[0].VolumeId' --output text 2>/dev/null | grep -v '^None$' || true)
    case "$lane" in
        fresh)
            [ -z "$OLD_VOL" ] || die "data volume ${VNAME} already exists (${OLD_VOL}); adopt it or retag."
            new_data_volume "$AZ" "$VNAME"
            ;;
        adopt)
            [ -n "$OLD_VOL" ] || die "no volume tagged ${VNAME} to adopt."
            echo "$OLD_VOL"
            ;;
        snapshot)
            SNAP=$(aws ec2 describe-snapshots --owner-ids self \
                       --filters "Name=tag:Name,Values=$VNAME" "Name=status,Values=completed" \
                       --query 'reverse(sort_by(Snapshots,&StartTime))[0].SnapshotId' \
                       --output text)
            [ -n "$SNAP" ] && [ "$SNAP" != "None" ] \
                || die "no completed snapshot for ${VNAME}; aborting (never create from None)."
            aws ec2 create-volume --snapshot-id "$SNAP" \
                --availability-zone "$AZ" --volume-type gp3 --encrypted \
                --tag-specifications \
                "ResourceType=volume,Tags=[{Key=Name,Value=$VNAME},$T]" \
                --query VolumeId --output text
            ;;
    esac
}

cmd_launch() {
    local lane=adopt az="" ami="" scratch=0
    while (( $# > 0 )); do
        case "$1" in
            --fresh-data)    lane=fresh ;;
            --adopt-data)    lane=adopt ;;
            --from-snapshot) lane=snapshot ;;
            --az) [ -n "${2:-}" ] || die "--az needs a value."; az="$2"; shift ;;
            --ami) [ -n "${2:-}" ] || die "--ami needs a value."; ami="$2"; shift ;;
            --scratch) scratch=1 ;;
            *) die "unknown launch option '$1'." ;;
        esac
        shift
    done
    clone_repo
    resolve_common
    resolve_golden_ami "$ami"
    az_pair
    AZ="${az:-$AZ1}"

    local INAME="$NAME" VNAME="$NAME-data"
    if (( scratch )); then INAME="$NAME-scratch"; VNAME="$NAME-scratch-data"; fi

    echo "== data volume (lane ${lane}) =="
    local DATA_VOL
    DATA_VOL=$(fn_scalar "data volume" lane_data_volume "$lane" "$VNAME" "$AZ")

    echo "== instance =="
    local SUBNET INSTANCE_ID TEMP_IP DOD HOST_MODE
    SUBNET=$(aws ec2 describe-subnets --filters "Name=vpc-id,Values=$VPC_ID" \
                 "Name=availability-zone,Values=$AZ" \
                 --query 'Subnets[0].SubnetId' --output text)
    [ -n "$SUBNET" ] && [ "$SUBNET" != "None" ] || die "no subnet in AZ ${AZ}."
    INSTANCE_ID=$(fn_scalar "run instance" launch_instance "$SUBNET" "$INAME")
    aws ec2 wait instance-status-ok --instance-ids "$INSTANCE_ID" \
        || die "instance never reached status-ok."
    if (( ! scratch )); then
        aws ec2 modify-instance-attribute --instance-id "$INSTANCE_ID" \
            --disable-api-termination || die "disable-api-termination failed."
    fi
    TEMP_IP=$(fn_scalar "instance ip" instance_ip "$INSTANCE_ID")
    attach_data "$DATA_VOL" "$INSTANCE_ID"
    DOD=$(aws ec2 describe-volumes --volume-ids "$DATA_VOL" --output text \
              --query 'Volumes[0].Attachments[0].DeleteOnTermination')
    [ "$DOD" = "False" ] || die "data volume DeleteOnTermination is ${DOD}; wanted False."
    HOST_MODE=fresh
    [ "$lane" = adopt ] && HOST_MODE=adopt
    [ "$lane" = snapshot ] && HOST_MODE=adopt
    echo "== post-image on ${TEMP_IP} (mode ${HOST_MODE}) =="
    $SSH "ubuntu@$TEMP_IP" "sudo bash -s -- post-image ${HOST_MODE} ${DATA_VOL}" \
        < "$REPO/ops/prod-2-host.sh" || die "post-image failed on the instance."
    echo
    echo "launched: ${INSTANCE_ID} at ${TEMP_IP} (volume ${DATA_VOL}, lane ${lane})."
    if (( scratch )); then
        echo "scratch: no Elastic IP (cutover refuses it by design)."
        echo "next: scripts/data-ui rebirth --fresh-data on the scratch host."
    else
        echo "next: rebirth on the host, then: $0 publish-eip"
    fi
}

# ---------------------------------------------------------------------------
# publish-eip — A7
# ---------------------------------------------------------------------------

cmd_publish_eip() {
    resolve_common
    local INSTANCE_ID GOT
    INSTANCE_ID=$(aws_tag_one "instance lookup" ec2 describe-instances \
                      --filters "Name=tag:Name,Values=$NAME" \
                      "Name=instance-state-name,Values=pending,running" \
                      --query 'Reservations[0].Instances[0].InstanceId' --output text)
    aws ec2 associate-address --allocation-id "$EIP_ALLOC" \
        --instance-id "$INSTANCE_ID" --allow-reassociation \
        || die "associate-address failed."
    GOT=$(aws ec2 describe-addresses --allocation-ids "$EIP_ALLOC" --output text \
              --query 'Addresses[0].InstanceId')
    [ "$GOT" = "$INSTANCE_ID" ] || die "EIP association verification failed (${GOT})."
    ssh-keygen -R "$EIP" >/dev/null 2>&1 || true
    cat >> ~/.ssh/config <<EOF

Host prod-2
    HostName $EIP
    User macnod
    IdentityFile ${KEY_PUB%.pub}
EOF
    ssh -o StrictHostKeyChecking=accept-new prod-2 true \
        || die "ssh prod-2 failed after the alias install."
    echo "Elastic IP ${EIP} -> ${INSTANCE_ID}; alias 'prod-2' verified."
}

# ---------------------------------------------------------------------------
# snapshots-policy / lifecycle / budget — A8..A10
# ---------------------------------------------------------------------------

cmd_snapshots_policy() {
    aws dlm create-default-role --resource-type snapshot 2>/dev/null || true # may exist
    cat > "$WORK/dlm.json" <<'JSON'
{"PolicyType": "EBS_SNAPSHOT_MANAGEMENT", "ResourceTypes": ["VOLUME"],
 "TargetTags": [{"Key": "Name", "Value": "data-ui-prod-2-data"}],
 "Schedules": [{"Name": "hourly", "CopyTags": true,
   "CreateRule": {"Interval": 1, "IntervalUnit": "HOURS",
                  "Times": ["00:10"]},
   "RetainRule": {"Count": 48}}]}
JSON
    local DLM_ROLE="arn:aws:iam::$ACCOUNT_ID:role/service-role/AWSDataLifecycleManagerDefaultRole"
    aws dlm create-lifecycle-policy --state ENABLED \
        --description 'data-ui prod-2 /data hourly' \
        --execution-role-arn "$DLM_ROLE" \
        --policy-details "file://$WORK/dlm.json" --tags Project="$PROJECT" \
        || die "create-lifecycle-policy failed."
    echo "hourly snapshots armed (follows Name=data-ui-prod-2-data; a retired volume must be retagged)."
}

cmd_lifecycle() {
    aws s3api get-bucket-lifecycle-configuration --bucket "$BUCKET" \
        > "$WORK/lifecycle-current.json" || die "cannot read the bucket lifecycle."
    if jq -e '[.Rules[].ID | select(startswith("prod-2-"))] | length == 0' \
        "$WORK/lifecycle-current.json" >/dev/null; then
        echo "no prod-2 rules yet"
    else
        die "prod-2 lifecycle rules already merged; refusing to merge twice."
    fi
    jq '{Rules: (.Rules
        + [.Rules[] | select((.Filter.Prefix // "") | startswith("evo-x2/"))
           | .ID = "prod-2-" + .ID
           | .Filter.Prefix |= sub("^evo-x2/"; "prod-2/")]
        + [{ID: "prod-2-site-noncurrent", Status: "Enabled",
            Filter: {Prefix: "prod-2/site/"},
            NoncurrentVersionExpiration: {NoncurrentDays: 90}}])}' \
        "$WORK/lifecycle-current.json" > "$WORK/lifecycle-merged.json"
    diff <(jq -S . "$WORK/lifecycle-current.json") \
         <(jq -S . "$WORK/lifecycle-merged.json") || true   # review, then:
    aws s3api put-bucket-lifecycle-configuration --bucket "$BUCKET" \
        --lifecycle-configuration "file://$WORK/lifecycle-merged.json" \
        || die "put-bucket-lifecycle-configuration failed."
    echo "prod-2/ lifecycle rules merged (every evo-x2/ rule cloned onto prod-2/)."
}

cmd_budget() {
    aws ce update-cost-allocation-tags-status \
        --cost-allocation-tags-status TagKey=Project,Status=Active 2>/dev/null \
        || echo "note: cost-allocation tag activation failed; retry within 24h of foundation." >&2
    : "${BUDGET_EMAIL:?set the billing-alert address (BUDGET_EMAIL=...)}"
    cat > "$WORK/budget.json" <<JSON
{"BudgetName": "data-ui-prod-2",
 "BudgetLimit": {"Amount": "200", "Unit": "USD"},
 "TimeUnit": "MONTHLY", "BudgetType": "COST",
 "CostFilters": {"TagKeyValue": ["user:Project\$data-ui-prod-2"]}}
JSON
    jq -n --arg e "$BUDGET_EMAIL" '[150, 200] | map({
      Notification: {NotificationType: "ACTUAL",
        ComparisonOperator: "GREATER_THAN", Threshold: .,
        ThresholdType: "ABSOLUTE_VALUE"},
      Subscribers: [{SubscriptionType: "EMAIL", Address: \$e}]})' \
        > "$WORK/budget-notifications.json"
    aws budgets create-budget --account-id "$ACCOUNT_ID" \
        --budget "file://$WORK/budget.json" \
        --notifications-with-subscribers "file://$WORK/budget-notifications.json" \
        || die "create-budget failed."
    echo "budget data-ui-prod-2 armed (\$200/month, tag-filtered, mail at 150/200)."
}

# ---------------------------------------------------------------------------
# replace — A11 (the AWS half of the failure ladder)
# ---------------------------------------------------------------------------

fence_old_instances() {
    # Stop EVERY matching instance, unconditionally: DescribeInstances
    # has no singleton or ordering guarantee, and a tag filter is not
    # one. The fence query's projection is parser-validated JMESPath
    # (after a filter projection, .[] followed by another [] is a
    # syntax error; Reservations[].Instances[?...][].[a,b] parses and
    # returns the id/state pairs); a multi-reservation fixture (two
    # reservations, one instance each, one shutting-down) is in the
    # A11 test matrix.
    local STRAGGLERS STAMP ID STATE RUNNING=0
    STAMP=$(date -u +%Y%m%dT%H%M%SZ)
    STRAGGLERS=$(aws ec2 describe-instances \
        --filters "Name=tag:Name,Values=$NAME" \
        --query 'Reservations[].Instances[?State.Name!=`terminated`][].[InstanceId,State.Name]' \
        --output text) || die "the fence query failed."
    if [[ -n "$STRAGGLERS" ]]; then
        while read -r ID STATE; do
            [ -n "$ID" ] || continue
            case "$STATE" in
                shutting-down)
                    aws ec2 wait instance-terminated --instance-ids "$ID" \
                        || { echo "prod-2-aws.sh: $ID failed to terminate; resolve by hand" >&2; exit 1; } ;;
                pending|running|stopping|stopped)
                    (( RUNNING += 1 ))
                    aws ec2 stop-instances --instance-ids "$ID" >/dev/null \
                        || { echo "prod-2-aws.sh: $ID failed to stop; resolve by hand" >&2; exit 1; }
                    aws ec2 wait instance-stopped --instance-ids "$ID" \
                        || { echo "prod-2-aws.sh: $ID failed to reach stopped; resolve by hand" >&2; exit 1; }
                    aws ec2 create-tags --resources "$ID" \
                        --tags Key=Name,Value="$NAME-retired-$STAMP" ;;
            esac
        done <<< "$STRAGGLERS"
        (( RUNNING <= 1 )) || {
            echo "prod-2-aws.sh: ${RUNNING} non-terminated instances matched $NAME before the fence; exactly one was expected; the extras are stopped and retired above — resolve by hand before continuing" >&2
            exit 1
        }
    else
        echo "no live instances matched ${NAME} (a terminated old instance is valid; fencing skips)."
    fi
}

cmd_replace() {
    local lane="" az=""
    while (( $# > 0 )); do
        case "$1" in
            --adopt-data|--from-snapshot|--fresh-data) lane="$1" ;;
            --az) [ -n "${2:-}" ] || die "--az needs a value."; az="$2"; shift ;;
            *) die "unknown replace option '$1'." ;;
        esac
        shift
    done
    [ -n "$lane" ] || die "replace needs exactly one lane: --adopt-data | --from-snapshot | --fresh-data."
    clone_repo
    resolve_common
    resolve_golden_ami
    az_pair
    AZ="${az:-}"

    echo "== fence (stop every matching instance; the old one is not special-cased) =="
    fence_old_instances

    local STAMP DATA_VOL OLD_VOL SNAP
    STAMP=$(date -u +%Y%m%dT%H%M%SZ)
    OLD_VOL=$(aws ec2 describe-volumes --filters "Name=tag:Name,Values=$NAME-data" \
                  --query 'Volumes[0].VolumeId' --output text 2>/dev/null | grep -v '^None$' || true)
    case "$lane" in
        --adopt-data)
            [ -n "$OLD_VOL" ] || die "old data volume not found; use the snapshot or fresh lane."
            AZ=$(aws ec2 describe-volumes --volume-ids "$OLD_VOL" \
                     --query 'Volumes[0].AvailabilityZone' --output text)
            aws ec2 create-snapshot --volume-id "$OLD_VOL" \
                --description "pre-adopt $STAMP" --tag-specifications \
                "ResourceType=snapshot,Tags=[{Key=Name,Value=$NAME-pre-adopt},$T]" >/dev/null \
                || die "the pre-adopt safety snapshot failed; aborting."
            aws ec2 detach-volume --volume-id "$OLD_VOL" || die "detach-volume failed (use --force by hand if stuck)."
            DATA_VOL="$OLD_VOL"
            ;;
        --from-snapshot)
            SNAP=$(aws ec2 describe-snapshots --owner-ids self \
                       --filters "Name=tag:Name,Values=$NAME-data" "Name=status,Values=completed" \
                       --query 'reverse(sort_by(Snapshots,&StartTime))[0].SnapshotId' \
                       --output text)
            [ -n "$SNAP" ] && [ "$SNAP" != "None" ] \
                || die "no completed snapshot for $NAME-data; aborting (never create from None)."
            : "${AZ:=$(aws ec2 describe-subnets --filters "Name=vpc-id,Values=$VPC_ID" \
                           --query 'sort_by(Subnets,&CidrBlock)[0].AvailabilityZone' --output text)}"
            [ -z "$OLD_VOL" ] || aws ec2 create-tags --resources "$OLD_VOL" \
                --tags Key=Name,Value="$NAME-data-retired-$STAMP" >/dev/null
            DATA_VOL=$(aws ec2 create-volume --snapshot-id "$SNAP" \
                           --availability-zone "$AZ" --volume-type gp3 --encrypted \
                           --tag-specifications \
                           "ResourceType=volume,Tags=[{Key=Name,Value=$NAME-data},$T]" \
                           --query VolumeId --output text)
            ;;
        --fresh-data)
            : "${AZ:=$(aws ec2 describe-subnets --filters "Name=vpc-id,Values=$VPC_ID" \
                           --query 'sort_by(Subnets,&CidrBlock)[0].AvailabilityZone' --output text)}"
            [ -z "$OLD_VOL" ] || aws ec2 create-tags --resources "$OLD_VOL" \
                --tags Key=Name,Value="$NAME-data-retired-$STAMP" >/dev/null
            DATA_VOL=$(new_data_volume "$AZ" "$NAME-data")
            ;;
    esac
    [ -n "$DATA_VOL" ] && [ "$DATA_VOL" != "None" ] || die "no data volume id in the ${lane} lane."

    local SUBNET INSTANCE_ID TEMP_IP HOST_MODE
    SUBNET=$(aws ec2 describe-subnets --filters "Name=vpc-id,Values=$VPC_ID" \
                 "Name=availability-zone,Values=$AZ" \
                 --query 'Subnets[0].SubnetId' --output text)
    [ -n "$SUBNET" ] && [ "$SUBNET" != "None" ] || die "no subnet in AZ ${AZ}."
    INSTANCE_ID=$(fn_scalar "run instance" launch_instance "$SUBNET" "$NAME")
    aws ec2 wait instance-status-ok --instance-ids "$INSTANCE_ID" \
        || die "instance never reached status-ok."
    aws ec2 modify-instance-attribute --instance-id "$INSTANCE_ID" \
        --disable-api-termination || die "disable-api-termination failed."
    TEMP_IP=$(fn_scalar "instance ip" instance_ip "$INSTANCE_ID")
    attach_data "$DATA_VOL" "$INSTANCE_ID"
    HOST_MODE=adopt
    [ "$lane" = "--fresh-data" ] && HOST_MODE=fresh
    $SSH "ubuntu@$TEMP_IP" "sudo bash -s -- post-image ${HOST_MODE} ${DATA_VOL}" \
        < "$REPO/ops/prod-2-host.sh" || die "post-image failed."
    local REBIRTH_FLAG="--$(echo "$lane" | tr - _ | sed 's/^--//; s/_data$/-data/')"
    $SSH "macnod@$TEMP_IP" \
        "/data/data-ui/repos/production/scripts/data-ui rebirth ${REBIRTH_FLAG}" \
        || die "rebirth failed on the replacement — the host is up at ${TEMP_IP}; run it by hand."
    echo
    echo "replacement ${INSTANCE_ID} is at ${TEMP_IP}."
    echo "Verify each app there (pinned /health, a login), THEN publish the Elastic IP:"
    echo "  $0 publish-eip"
    echo "The retired instance and volumes are kept (retagged) until Donnie approves retire."
}

# ---------------------------------------------------------------------------
# ssh-allow / add-app / retire
# ---------------------------------------------------------------------------

cmd_ssh_allow() {
    local VPC_ID SG_ID MY
    VPC_ID=$(find_vpc); [ -n "$VPC_ID" ] || die "no VPC; run foundation first."
    SG_ID=$(find_sg);   [ -n "$SG_ID" ]  || die "no security group; run foundation first."
    MY="$(curl -fsS https://checkip.amazonaws.com)/32"
    local CIDR
    while read -r CIDR; do
        [ -n "$CIDR" ] || continue
        aws ec2 revoke-security-group-ingress --group-id "$SG_ID" \
            --protocol tcp --port 22 --cidr "$CIDR" >/dev/null 2>&1 || true
    done < <(aws ec2 describe-security-groups --group-ids "$SG_ID" \
                 --query 'SecurityGroups[0].IpPermissions[?FromPort==`22`].IpRanges[].CidrIp' \
                 --output text)
    aws ec2 authorize-security-group-ingress --group-id "$SG_ID" \
        --protocol tcp --port 22 --cidr "$MY" || die "authorize failed."
    echo "ssh now allowed from ${MY} only."
}

cmd_add_app() {
    local app="$1" domain="$2"
    [ -n "$app" ] && [ -n "$domain" ] || die "usage: add-app <name> <domain>."
    APPS="$(echo "$APPS $app" | tr ' ' '\n' | awk 'NF' | sort -u | tr '\n' ' ')"
    write_role_policy
    aws iam put-role-policy --role-name "$NAME" --policy-name site \
        --policy-document "file://$WORK/role-policy.json" \
        || die "re-putting the role policy failed."
    if ! aws ecr describe-repositories --repository-names "data-ui/$app" >/dev/null 2>&1; then
        aws ecr create-repository --repository-name "data-ui/$app" \
            --image-tag-mutability MUTABLE \
            --image-scanning-configuration scanOnPush=true \
            --encryption-configuration encryptionType=AES256 \
            --tags Key=Project,Value="$PROJECT" || die "create-repository failed."
    fi
    ecr_lifecycle_file
    aws ecr put-lifecycle-policy --repository-name "data-ui/$app" \
        --lifecycle-policy-text "file://$WORK/ecr-lifecycle.json" \
        || die "put-lifecycle-policy failed."
    echo "added ${app} (${domain}): role policy widened, ECR repository ready."
}

cmd_retire() {
    local id="$1"
    [ -n "$id" ] || die "usage: retire <instance-id>."
    aws ec2 modify-instance-attribute --instance-id "$id" \
        --no-disable-api-termination || die "clearing termination protection failed."
    aws ec2 terminate-instances --instance-ids "$id" >/dev/null \
        || die "terminate-instances failed."
    echo "retiring ${id}."
}

case "${1:-}" in
    foundation)       cmd_foundation ;;
    bake)             cmd_bake ;;
    launch)           shift; cmd_launch "$@" ;;
    publish-eip)      cmd_publish_eip ;;
    snapshots-policy) cmd_snapshots_policy ;;
    lifecycle)        cmd_lifecycle ;;
    budget)           cmd_budget ;;
    replace)          shift; cmd_replace "$@" ;;
    ssh-allow)        cmd_ssh_allow ;;
    add-app)          shift; cmd_add_app "${1:-}" "${2:-}" ;;
    retire)           shift; cmd_retire "${1:-}" ;;
    *)
        sed -n '2,44p' "$0" >&2
        exit 1
        ;;
esac
