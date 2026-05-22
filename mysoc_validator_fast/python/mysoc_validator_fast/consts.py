from enum import Enum


class StrEnum(str, Enum):
    def __str__(self) -> str:
        return str(self.value)


class Chamber(StrEnum):
    COMMONS = "house-of-commons"
    LORDS = "house-of-lords"
    SCOTLAND = "scottish-parliament"
    SENEDD = "welsh-parliament"
    LONDON = "london-assembly"
    NORTHERN_IRELAND = "northern-ireland-assembly"


class IdentifierScheme(StrEnum):
    DATADOTPARL = "datadotparl_id"
    MNIS = "datadotparl_id"
    PIMS = "pims_id"
    HISTORIC_HANSARD = "historichansard_id"
    PEERAGE_TYPE = "peeragetype"
    WIKIDATA = "wikidata"
    SCOTPARL = "scotparl_id"
    SENEDD = "senedd"
    NI_ASSEMBLY = "data.niassembly.gov.uk"


class MembershipReason(StrEnum):
    BLANK = ""
    ACCESSION = "accession"
    APPOINTED = "appointed"
    BECAME_PEER = "became_peer"
    BECAME_PRESIDING_OFFICER = "became_presiding_officer"
    BY_ELECTION = "by_election"
    CHANGED_PARTY = "changed_party"
    DECLARED_VOID = "declared_void"
    DEVOLUTION = "devolution"
    DIED = "died"
    DISQUALIFIED = "disqualified"
    DISSOLUTION = "dissolution"
    ELECTION = "election"
    GENERAL_ELECTION = "general_election"
    GENERAL_ELECTION_NOT_STANDING = "general_election_not_standing"
    GENERAL_ELECTION_PROBABLY = "general_election_probably"
    GENERAL_ELECTION_STANDING = "general_election_standing"
    RECALL_PETITION = "recall_petition"
    REGIONAL_ELECTION = "regional_election"
    REINSTATED = "reinstated"
    REPLACED_IN_REGION = "replaced_in_region"
    RESIGNED = "resigned"
    RETIRED = "retired"
    WHIP_REMOVED = "whip_removed"
    WHIP_RESTORED = "whip_restored"
    UNKNOWN = "unknown"
