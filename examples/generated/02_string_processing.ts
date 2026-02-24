function main(): void {

  const NAME: string = "FerroPhase";

  const VERSION: string = "0.1.0";

  const NAME_LEN: number = 10;

  const VERSION_LEN: number = 5;
  console.log(`name='${NAME}' len=${NAME_LEN}`);
  console.log(`version='${VERSION}' len=${VERSION_LEN}`);

  const IS_EMPTY: boolean = false;

  const IS_LONG: boolean = true;
  console.log(`empty=${IS_EMPTY}, long=${IS_LONG}`);

  const BANNER: string = "FerroPhase v0.1.0";
  console.log(`banner='${BANNER}'`);

  const BUFFER_SIZE: number = 256;
  console.log(`buffer_size=${BUFFER_SIZE}`);
}

main();
