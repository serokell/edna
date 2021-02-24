// The module might be removed if we start using open api client generator

export interface MeasurementDto {
  compoundId: string;
  concentration: number;
  signal: number;
}

export type CompoundsMap = { [compoundId: string]: MeasurementDto[] };

export function groupCompounds(mes: MeasurementDto[]): CompoundsMap {
  return mes.reduce((rv: CompoundsMap, x) => {
    // eslint-disable-next-line no-param-reassign
    (rv[x.compoundId] = rv[x.compoundId] || []).push(x);
    return rv;
  }, {});
}
