#!/bin/bash
# publish to web
git=git
repo=github.com/StanTraykov/C19_BG
user=StanTraykov
giturl="https://${user}:${pass}@${repo}"
rdir="/c/Users/Stan/Documents/webc19/C19_BG"
copy_dir="${rdir}/cur_svg"
git_push="${git} push"
git_fetch="${git} fetch"
git_pull="${git} pull"
git_stage="${git} add ."
git_commit="${git} commit -m"
tday=$(date +%b%d)
yday=$(date -d "${tday}-1day" +%b%d)
c_date=$(date "+%b%d %T")
if [[ -d "${tday}" ]]; then
    src_dir="${tday}"
elif [[ -d "${yday}" ]]; then
    src_dir="${yday}"
else
    echo "can't find suitable source dir (tried ${tday}, ${yday})" >&2
    exit 1
fi
pub_files="${src_dir}/svg/C*.svg ${src_dir}/svg/D00*.svg ${src_dir}/svg/D*BG.svg ${src_dir}/png/C*heat.png"
cp -a ${pub_files} "${copy_dir}"
cd "${copy_dir}" || exit 1
${git_fetch} && ${git_pull} && ${git_stage} && ${git_commit} "auto upload ${src_dir} (${c_date})"
# also update index.md with the last update time
mdate=$(${git} log --date "iso-local" -n 1 . | grep Date: | sed -e 's/Date: *//')
cd ..
regpre='s/(<!-- up -->).*?(<!-- date -->)/'
sed -i -re "${regpre}\1${mdate}\2/" index.md
${git_stage} && ${git_commit} "auto update time ${src_dir} (${c_date})"
${git_push} "$@"
