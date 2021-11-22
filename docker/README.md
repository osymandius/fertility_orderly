## Instructions for running on server

To run a script on a server we run inside a docker image to avoid installing a bunch of deps on the server.

1. Start screen session - in case your session times out, we don't want to lose progress
2. Build the container `./docker/build`
3. Run the script inside the container with a mount to access local files
```
docker run -d \
       -e SHAREPOINT_URL=https://imperiallondon.sharepoint.com/ \
       -e SHAREPOINT_SITE=HIVInferenceGroup-WP \
       -e SHAREPOINT_USERNAME=<email> \
       -e SHAREPOINT_PASS=<pass> \
       --mount type=bind,source=$(realpath .),target=/fertility_orderly \
       -w /fertility_orderly \
       osymandius/fertilityorderly:<git_sha> Rscript rob.R
```
4. Collect stats via `./docker/bin/collect_memory_usage.sh &`
5. After complete send output, memory stats and logs back via rsync. Find path to logs from `docker inspect --format='{{.LogPath}}' <containername>`