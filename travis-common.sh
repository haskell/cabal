set -e

HACKAGE_REPO_TOOL_VERSION="0.1.1"
CABAL_VERSION="2.1.0.0"

# ---------------------------------------------------------------------
# Timing / diagnostic output
# ---------------------------------------------------------------------

JOB_START_TIME=$(date +%s)

timed() {
    echo "\$ $*"
    start_time=$(date +%s)

	# Run the job
    $* || exit $?

	# Calculate the durations
    end_time=$(date +%s)
    duration=$((end_time - start_time))
	total_duration=$((end_time - JOB_START_TIME))

	# Print them
    echo "$* took $duration seconds."
    echo "whole job took $total_duration seconds so far."

	# Terminate on OSX
	if [ $total_duration -ge 2400 -a $(uname) = "Darwin" ]; then
		echo "Job taking over 40 minutes. Terminating"
		exit 1
	fi
    echo "----"
}
