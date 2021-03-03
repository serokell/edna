// The module might be removed if we start using open api client generator

export interface MeasurementDto {
  compoundId: string;
  concentration: number;
  signal: number;
}

export type CompoundsMap = { [compoundId: string]: MeasurementDto[] };

export function groupCompounds(mes: MeasurementDto[]): CompoundsMap {
  return mes.reduce((groups: CompoundsMap, x) => {
    const group = groups[x.compoundId] || [];
    group.push(x);
    // eslint-disable-next-line no-param-reassign
    groups[x.compoundId] = group;
    return groups;
  }, {});
}
