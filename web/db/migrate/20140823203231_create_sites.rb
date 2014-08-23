class CreateSites < ActiveRecord::Migration
  def change
    create_table :sites do |t|
      t.integer :clan_id
      t.timestamps
    end
  end
end
