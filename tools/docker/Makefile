platforms := linux/amd64 #linux/arm64/v8

all: $(platforms)

$(platforms):
	docker buildx build --platform $@ -f Dockerfile.ubuntu \
	-t ktakashi/sagittarius:latest .
	$(eval version=$(shell docker run -it ktakashi/sagittarius \
	-e'(display (sagittarius-version))(newline)(exit)' 2>/dev/null))
	@echo $(version)
	docker image tag ktakashi/sagittarius:latest ktakashi/sagittarius:$(version)
	docker push ktakashi/sagittarius:latest
	docker push ktakashi/sagittarius:$(version)

linux/arm64/v8:
	docker buildx build --platform $@ -f Dockerfile.ubuntu \
	-t ktakashi/sagittarius:$(version)-arm64 \
	-t ktakashi/sagittarius:latest-arm64
