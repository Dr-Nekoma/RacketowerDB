CREATE RELATION Person(
    age INTEGER CHECK age >= 0 && age != 5,
    name VARCHAR(64),
    document_number DOCUMENT_NUMBER document_number = (name + (DATE_NOW() - age)),
    KEY(document_number),
    UNIQUE (age, name, document_number),
    CHECK function_check_if_no_person_with_age_5()
);

KEY -> HASH document_number -> rowId
UNIQUE -> HASH (age, name, document_number) -> rowId

RowId = 1 (1,"Lemos","Lemos-2022")
RowId = 2 (1,"Marinho","Marinho-2022")
RowId = 3 (1,"Magueta","Magueta-2022")

