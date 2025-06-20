platforms := 'linux/amd64,linux/arm64/v8,linux/arm/v7,linux/ppc64le,linux/riscv64'
VERSION ?= latest

all: build

build:
	docker buildx build --platform $(platforms) -f Dockerfile.ubuntu \
	  --build-arg VERSION=$(VERSION) -t ktakashi/sagittarius:$(VERSION) .

push: build push_$(VERSION)

push_$(VERSION):
	docker push ktakashi/sagittarius:$(VERSION)

push_latest: build_version
	docker image tag ktakashi/sagittarius:latest ktakashi/sagittarius:$(build_version)
	docker push ktakashi/sagittarius:latest
	docker push ktakashi/sagittarius:$(build_version)

build_version:
	$(eval build_version=$(shell docker run -it ktakashi/sagittarius:$(VERSION) \
	  -e'(display (sagittarius-version))(newline)(exit)' 2>/dev/null))
	@echo $(build_version)
