class CreateSiteAdmins < ActiveRecord::Migration
  def change
    create_table :site_admins do |t|
      t.integer :clan_id
      t.integer :account_id

      t.timestamps
    end
    add_index :site_admins, :clan_id
    add_index :site_admins, :account_id
  end
end
