# Edna deployment

The [docker-compose configuration file](./docker-compose.yml) declares how to deploy Edna using images from GitHub Container Registry:
* [backend](https://github.com/orgs/serokell/packages/container/package/edna-backend)
* [frontend](https://github.com/orgs/serokell/packages/container/package/edna-frontend)

You can use `docker-compose` to run all 3 images (`postgres`, `backend` and `frontend`) or any individual image.
For example:
* `docker-compose up postgres` to run `postgres` in the foreground.
* `docker-compose up --no-start` to create all services without running them.
* `docker-compose start` to run all services in the background.

If you run all 3 images (by not passing any image or passing `frontend`), you can open `127.0.0.1:8080` in your browser and use Edna.
Note that `localhost` is handled differently by `nginx` Docker image, so `localhost:8080` may work incorrectly.

By default `docker-compose` will pull Edna images from GitHub Container Registry.
However, if you import an image manually, it will be used instead.
You can use `docker-compose pull` to update all images.
