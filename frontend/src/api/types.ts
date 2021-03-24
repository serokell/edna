// The module might be removed if we start using open api client generator

export interface MeasurementDto {
  compoundId: string;
  targetId: string;
  concentration: number;
  signal: number;
  outlier: boolean;
}

export type ParsedTargetDto = {
  isNew: boolean;
  target: string;
  compounds: string[];
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
interface Dto<T, Extra> {
  id: number;
  item: T;
  extra: Extra;
}

interface ProjectBody {
  name: string;
  description?: string;
}

export type Timestamp = number;

interface ProjectExtra {
  creationDate: Timestamp;
  lastUpdate: Timestamp;
  compoundNames: string[];
}

export type ProjectDto = Dto<ProjectBody, ProjectExtra>;

interface NameWithProjects {
  name: string;
  projects: string[];
}

export type TargetDto = {
  id: number;
  item: NameWithProjects;
  creationDate: Timestamp;
};

export type CompoundDto = {
  id: number;
  item: NameWithProjects;
  creationDate: Timestamp;
};

// TODO remove these random generators
export function genRandomProject(): ProjectDto {
  return {
    id: randomInt(1000000000),
    item: {
      name: randomString(randomPos(20)),
      description: randomString(randomInt(200)),
    },
    extra: {
      creationDate: randomInt(2000000000000),
      lastUpdate: randomInt(2000000000000),
      compoundNames: genSeq(randomInt(10), () => randomString(randomPos(10))),
    },
  };
}

export function genRandomTarget(): TargetDto {
  return {
    id: randomInt(1000000000),
    item: {
      name: randomString(randomPos(20)),
      projects: genSeq(randomInt(5), genRandomProject).map(x => x.item.name),
    },
    creationDate: randomInt(2000000000000),
  };
}

export function genSeq<T>(size: number, gen: () => T): T[] {
  const a = [];
  for (let i = 0; i < size; ++i) a.push(gen());
  return a;
}

function randomInt(rightBound: number): number {
  return Math.floor(Math.random() * rightBound);
}

function randomRange(l: number, r: number) {
  return Math.floor(Math.random() * (r - l + 1)) + l;
}

function randomPos(r: number) {
  return randomRange(1, r);
}

function randomString(size: number): string {
  const alphaChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  let generatedString = "";
  for (let i = 0; i < size; i++) {
    generatedString += alphaChars[randomInt(alphaChars.length)];
  }

  return generatedString;
}
