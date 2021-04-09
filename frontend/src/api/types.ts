// The module might be removed if we start using open api client generator

export interface MeasurementDto {
  compoundId: string;
  targetId: string;
  concentration: number;
  signal: number;
  outlier: boolean;
}

type ParsedCompoundDto = {
  id?: number;
  name: string;
};

type ParsedTargetDto = {
  id?: number;
  name: string;
};

export type ParsedExcelDto = {
  target: ParsedTargetDto;
  compounds: ParsedCompoundDto[];
};

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

// Data transfer object
interface Dto<T, Extra = undefined> {
  id: number;
  item: T;
  extra?: Extra;
}

interface ProjectBodyDto {
  name: string;
  description?: string;
  creationDate: DateTimeDto;
  lastUpdate: DateTimeDto;
  compoundNames: string[];
}

export type DateTimeDto = string;

export type ProjectDto = Dto<ProjectBodyDto>;

interface MethodologyBodyDto {
  name: string;
  description?: string;
  confluence?: string;
  projects: string[];
}

export type MethodologyDto = Dto<MethodologyBodyDto>;

export type TargetDto = {
  id: number;
  item: {
    name: string;
    projects: string[];
    additionDate: DateTimeDto;
  };
};

export type CompoundDto = {
  id: number;
  item: {
    name: string;
    chemSoft?: string;
    additionDate: DateTimeDto;
  };
};
