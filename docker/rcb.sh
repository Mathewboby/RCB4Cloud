WORK_DIR=/tmp/work
echo "Work directory being used is "$WORK_DIR/$1
#IMAGE=docker-registry.science-at-scale.io/rcb:prod
IMAGE=rcb:prod

docker run -i -v $WORK_DIR/$1:/work -v $WORK_DIR/$1/logs:/logs --rm ${IMAGE} Rscript -e 'library(methods);library(jsonlite, warn.conflicts=FALSE);library(RCB); input <- fromJSON("/work/input.json"); output = R.ASReml_RCB_Return(input$data,input$analysis_type); print(output) ; write(toJSON(output,auto_unbox=TRUE, pretty = TRUE),"/work/output.json")'
