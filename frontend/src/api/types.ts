// The module might be removed if we start using open api client generator

export interface MeasurementDto {
  compoundId: string;
  targetId: string;
  concentration: number;
  signal: number;
  outlier: boolean;
}

export type CompoundsMap = { [compoundId: string]: MeasurementDto[] };

export function groupCompounds(mes: MeasurementDto[]): CompoundsMap {
  const filteredCompounds = mes.filter(m => !m.outlier);
  return filteredCompounds.reduce((groups: CompoundsMap, x) => {
    const id = `${x.compoundId} ${x.targetId}`;
    const group = groups[id] || [];
    group.push(x);
    // eslint-disable-next-line no-param-reassign
    groups[id] = group;
    return groups;
  }, {});
}
