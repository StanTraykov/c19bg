#!/bin/bash
# publish to web
gsed=gsed #GNU sed
git=git
src_parent="c19bg/plots"
#rdir="/c/Users/Stan/Documents/webc19/C19_BG"
rdir="/Users/stan/webc19/c19bg"
copy_dir="${rdir}/cur_svg"
git_push="${git} push"
git_fetch="${git} fetch"
git_pull="${git} pull"
git_stage="${git} add ."
git_stage_docs="${git} add ../docs"
git_commit="${git} commit -m"
git_cur_time="${git} log --date iso-local -n 1 ."
tday=$(date +%b%d)
#yday=$(date -d "${tday}-1day" +%b%d) #not sup on macOS
c_date=$(date "+%b%d %T")
tday="${src_parent}/${tday}"
# yday="${src_parent}/${yday}"
# if [[ -d "${tday}" ]]; then
#     src_dir="${tday}"
# elif [[ -d "${yday}" ]]; then
#     src_dir="${yday}"
# else
#     echo "can't find suitable source dir (tried ${tday}, ${yday})" >&2
#     exit 1
# fi
if [[ -d "${tday}" ]]; then
    src_dir="${tday}"
else
    echo "can't find suitable source dir (tried ${tday})" >&2
    exit 1
fi

pub_files="${src_dir}/svg/C*.svg ${src_dir}/svg/D00*.svg ${src_dir}/svg/D*BG.svg ${src_dir}/png/C*heat*.png"
cp -a ${pub_files} "${copy_dir}"
cd "${copy_dir}" || exit 1
# macOS svglite output contains top-level height and width attribs
# which prevent auto-scaling in browsers, so we remove them
find *.svg | xargs -n 1 ${gsed} -i -re "s/(class='svglite') width='.*?pt' height='.*?pt'/\1/"
${git_stage_docs}
${git_fetch} && ${git_pull} && ${git_stage} && ${git_commit} "auto upload ${src_dir} (${c_date})"
# also update index.md with the last update time
mdate=$(${git_cur_time} | grep Date: | ${gsed} -e 's/Date: *//')
cd ..
regpre='s/(<!-- up -->).*?(<!-- date -->)/'
${gsed} -i -re "${regpre}\1${mdate}\2/" index.md
${git_stage} && ${git_commit} "auto update ${src_dir} (${c_date})"
${git_push} "$@"
