function main(): void {

  const TEMP: number = 25;

  const WEATHER: string = "warm";
  console.log(`${TEMP}°C is ${WEATHER}`);

  const IS_SUNNY: boolean = true;

  const IS_WARM: boolean = true;

  const ACTIVITY: string = "outdoor";
  console.log(`Suggested: ${ACTIVITY}`);

  const SCORE: number = 85;

  const GRADE: string = "B";
  console.log(`Score ${SCORE} = grade ${GRADE}`);
  let value = 42;
  let category = ((value > 50) ? "high" : ((value > 25) ? "medium" : "low"));
  console.log(`Value ${value} is ${category}`);
}

main();
